(* Command-line pricer.
   dune exec bin/main.exe -- --spot 100 --strike 105 --rate 0.05 --vol 0.2 --expiry 0.5 --put *)

open Pricer

let () =
  let spot = ref 100.0 and strike = ref 100.0 and rate = ref 0.05 and dividend = ref 0.0 in
  let vol = ref 0.2 and expiry = ref 1.0 and kind = ref Contract.Call and steps = ref 1000 in
  let specs =
    [
      ("--spot", Arg.Set_float spot, " underlying price (default 100)");
      ("--strike", Arg.Set_float strike, " strike (default 100)");
      ("--rate", Arg.Set_float rate, " risk-free rate, continuous (default 0.05)");
      ("--dividend", Arg.Set_float dividend, " dividend yield, continuous (default 0)");
      ("--vol", Arg.Set_float vol, " volatility (default 0.2)");
      ("--expiry", Arg.Set_float expiry, " years to expiry (default 1)");
      ("--put", Arg.Unit (fun () -> kind := Contract.Put), " price a put instead of a call");
      ("--steps", Arg.Set_int steps, " binomial steps (default 1000)");
    ]
  in
  Arg.parse (Arg.align specs) (fun _ -> ()) "pricer [options]";
  let o =
    Contract.make ~spot:!spot ~strike:!strike ~rate:!rate ~dividend:!dividend ~vol:!vol
      ~expiry:!expiry ()
  in
  let k = !kind in
  let bs = Black_scholes.price k o in
  let g = Black_scholes.greeks k o in
  let euro = Binomial.price ~steps:!steps Binomial.European k o in
  let amer = Binomial.price ~steps:!steps Binomial.American k o in
  let mc, se = Monte_carlo.price k o in
  Printf.printf "European %s  S=%g K=%g r=%g q=%g vol=%g T=%g\n\n" (Contract.kind_to_string k)
    o.spot o.strike o.rate o.dividend o.vol o.expiry;
  Printf.printf "  Black-Scholes          %10.6f\n" bs;
  Printf.printf "  CRR tree (%d steps)  %10.6f\n" !steps euro;
  Printf.printf "  Monte Carlo            %10.6f  +/- %.6f\n" mc se;
  Printf.printf "  American (CRR)         %10.6f  (early-exercise premium %.6f)\n\n" amer
    (amer -. euro);
  Printf.printf "  delta %9.5f   gamma %9.5f   vega %9.5f   theta %9.5f   rho %9.5f\n" g.delta
    g.gamma g.vega g.theta g.rho;
  match Implied_vol.solve k o ~price:bs with
  | Ok v -> Printf.printf "\n  implied vol of the BS price: %.12f\n" v
  | Error e -> Printf.printf "\n  implied vol failed: %s\n" e
