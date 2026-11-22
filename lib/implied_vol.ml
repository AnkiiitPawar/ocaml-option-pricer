(* Implied volatility: the sigma at which Black-Scholes reproduces a quoted price.

   Newton's method on sigma converges quadratically near the root but can shoot
   off when vega is tiny (deep in or out of the money). So we keep a bracket
   [lo, hi] that always contains the root (the price is increasing in sigma) and
   fall back to bisection whenever a Newton step would leave it. *)

open Contract

let bounds kind o =
  let df_r = exp (-.o.rate *. o.expiry) and df_q = exp (-.o.dividend *. o.expiry) in
  match kind with
  | Call -> (Float.max ((o.spot *. df_q) -. (o.strike *. df_r)) 0.0, o.spot *. df_q)
  | Put -> (Float.max ((o.strike *. df_r) -. (o.spot *. df_q)) 0.0, o.strike *. df_r)

(* Stops when the volatility itself has converged (step below [tol]), not when the
   price error is small. For far out-of-the-money options the price can be 1e-9
   while vega is 1e-6, so an absolute price tolerance would stop at the wrong vol. *)
let solve ?(tol = 1e-14) ?(max_iter = 200) kind o ~price =
  let lower, upper = bounds kind o in
  if not (price > lower && price < upper) then Error "price is outside the no-arbitrage bounds"
  else
    let f vol = Black_scholes.price kind { o with vol } -. price in
    let rec go iter vol lo hi =
      if iter > max_iter then Error "no convergence"
      else
        let fv = f vol in
        if fv = 0.0 then Ok vol
        else
          let lo, hi = if fv > 0.0 then (lo, vol) else (vol, hi) in
          let vega = Black_scholes.vega { o with vol } in
          let newton = vol -. (fv /. vega) in
          let next = if vega > 0.0 && newton > lo && newton < hi then newton else 0.5 *. (lo +. hi) in
          if Float.abs (next -. vol) < tol || hi -. lo < tol then Ok next else go (iter + 1) next lo hi
    in
    go 0 0.25 1e-8 10.0
