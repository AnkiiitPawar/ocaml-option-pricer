open Pricer

let failures = ref 0

let check name ok =
  Printf.printf "%s %s\n" (if ok then "PASS" else "FAIL") name;
  if not ok then incr failures

let close ?(tol = 1e-9) a b = Float.abs (a -. b) <= tol *. (1.0 +. Float.abs b)
let base = Contract.make ~spot:100.0 ~strike:100.0 ~rate:0.05 ~vol:0.2 ~expiry:1.0 ()

let with_dividend =
  Contract.make ~spot:100.0 ~strike:95.0 ~rate:0.03 ~dividend:0.02 ~vol:0.3 ~expiry:0.75 ()

let () =
  (* Textbook value: S = K = 100, r = 5 %, sigma = 20 %, T = 1  ->  call = 10.4506 *)
  check "reference Black-Scholes call value"
    (Float.abs (Black_scholes.price Call base -. 10.450584) < 1e-6)

let () =
  let ok = ref true in
  List.iter
    (fun strike ->
      List.iter
        (fun expiry ->
          let o = { with_dividend with strike; expiry } in
          let lhs = Black_scholes.price Call o -. Black_scholes.price Put o in
          let rhs =
            (o.spot *. exp (-.o.dividend *. expiry)) -. (strike *. exp (-.o.rate *. expiry))
          in
          if not (close ~tol:1e-12 lhs rhs) then ok := false)
        [ 0.1; 0.5; 1.0; 3.0 ])
    [ 60.0; 90.0; 100.0; 110.0; 150.0 ];
  check "put-call parity holds across strikes and maturities" !ok

let () =
  (* Every analytic Greek against a central finite difference of the price. *)
  let h = 1e-4 in
  List.iter
    (fun kind ->
      let o = with_dividend in
      let v o' = Black_scholes.price kind o' in
      let g = Black_scholes.greeks kind o in
      let fd_delta = (v { o with spot = o.spot +. h } -. v { o with spot = o.spot -. h }) /. (2.0 *. h) in
      let fd_gamma =
        (v { o with spot = o.spot +. h } -. (2.0 *. v o) +. v { o with spot = o.spot -. h }) /. (h *. h)
      in
      let fd_vega = (v { o with vol = o.vol +. h } -. v { o with vol = o.vol -. h }) /. (2.0 *. h) in
      let fd_theta =
        -.(v { o with expiry = o.expiry +. h } -. v { o with expiry = o.expiry -. h }) /. (2.0 *. h)
      in
      let fd_rho = (v { o with rate = o.rate +. h } -. v { o with rate = o.rate -. h }) /. (2.0 *. h) in
      let name s = Printf.sprintf "%s %s matches finite difference" (Contract.kind_to_string kind) s in
      check (name "delta") (close ~tol:1e-7 g.delta fd_delta);
      check (name "gamma") (close ~tol:1e-4 g.gamma fd_gamma);
      check (name "vega") (close ~tol:1e-7 g.vega fd_vega);
      check (name "theta") (close ~tol:1e-6 g.theta fd_theta);
      check (name "rho") (close ~tol:1e-7 g.rho fd_rho))
    [ Contract.Call; Contract.Put ]

let () =
  let exact = Black_scholes.price Put with_dividend in
  let err n = Float.abs (Binomial.price ~steps:n Binomial.European Put with_dividend -. exact) in
  check "binomial tree converges to Black-Scholes" (err 2000 < 2e-3 && err 2000 < err 250)

let () =
  (* Without dividends, exercising a call early is never optimal: American = European. *)
  let am = Binomial.price Binomial.American Call base and eu = Binomial.price Binomial.European Call base in
  check "American call on a non-dividend stock equals the European call" (close ~tol:1e-12 am eu)

let () =
  let o = { base with strike = 110.0 } in
  let am = Binomial.price Binomial.American Put o and eu = Binomial.price Binomial.European Put o in
  check "American put carries an early-exercise premium" (am > eu +. 0.05)

let () =
  (* Implied vol is only identifiable when the option has time value. At 5 % vol a
     call struck at 70 is worth its intrinsic value to ~1e-20, so in double
     precision there is nothing left to invert. Those cases are skipped. *)
  let ok = ref true and tested = ref 0 in
  List.iter
    (fun kind ->
      List.iter
        (fun strike ->
          List.iter
            (fun vol ->
              let o = { with_dividend with strike; vol } in
              let price = Black_scholes.price kind o in
              let lower, _ = Implied_vol.bounds kind o in
              if price -. lower > 1e-6 then begin
                incr tested;
                match Implied_vol.solve kind { o with vol = 0.5 } ~price with
                | Ok v when Float.abs (v -. vol) < 1e-8 -> ()
                | _ -> ok := false
              end)
            [ 0.05; 0.1; 0.3; 0.6; 1.0 ])
        [ 70.0; 90.0; 100.0; 110.0; 130.0 ])
    [ Contract.Call; Contract.Put ];
  check
    (Printf.sprintf "implied volatility round-trips to 1e-8 (%d identifiable cases)" !tested)
    (!ok && !tested >= 40)

let () =
  let arbitrage = Implied_vol.solve Call base ~price:150.0 in
  check "implied volatility rejects arbitrage prices" (Result.is_error arbitrage)

let () =
  let exact = Black_scholes.price Call with_dividend in
  let mc, se = Monte_carlo.price ~paths:400_000 Call with_dividend in
  check "Monte Carlo agrees within 4 standard errors" (Float.abs (mc -. exact) < 4.0 *. se)

let () =
  if !failures > 0 then (
    Printf.printf "%d failure(s)\n" !failures;
    exit 1)
  else print_endline "all tests passed"
