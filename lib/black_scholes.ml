(* Black-Scholes-Merton closed form with a continuous dividend yield, plus Greeks. *)

open Contract

let d1 o =
  (log (o.spot /. o.strike) +. ((o.rate -. o.dividend +. (0.5 *. o.vol *. o.vol)) *. o.expiry))
  /. (o.vol *. sqrt o.expiry)

let d2 o = d1 o -. (o.vol *. sqrt o.expiry)

let price kind o =
  let df_r = exp (-.o.rate *. o.expiry) and df_q = exp (-.o.dividend *. o.expiry) in
  match kind with
  | Call -> (o.spot *. df_q *. Normal.cdf (d1 o)) -. (o.strike *. df_r *. Normal.cdf (d2 o))
  | Put -> (o.strike *. df_r *. Normal.cdf (-.d2 o)) -. (o.spot *. df_q *. Normal.cdf (-.d1 o))

(* Vega is the same for calls and puts. *)
let vega o = o.spot *. exp (-.o.dividend *. o.expiry) *. Normal.pdf (d1 o) *. sqrt o.expiry

type greeks = {
  delta : float;  (* dV/dS *)
  gamma : float;  (* d2V/dS2 *)
  vega : float;  (* dV/dsigma *)
  theta : float;  (* dV/dt: value lost per year as time passes *)
  rho : float;  (* dV/dr *)
}

let greeks kind o =
  let sq = sqrt o.expiry in
  let d1 = d1 o and d2 = d2 o in
  let df_r = exp (-.o.rate *. o.expiry) and df_q = exp (-.o.dividend *. o.expiry) in
  let n_d1 = Normal.pdf d1 in
  let gamma = df_q *. n_d1 /. (o.spot *. o.vol *. sq) in
  let vega = o.spot *. df_q *. n_d1 *. sq in
  let decay = -.(o.spot *. df_q *. n_d1 *. o.vol) /. (2.0 *. sq) in
  match kind with
  | Call ->
      {
        delta = df_q *. Normal.cdf d1;
        gamma;
        vega;
        theta =
          decay
          -. (o.rate *. o.strike *. df_r *. Normal.cdf d2)
          +. (o.dividend *. o.spot *. df_q *. Normal.cdf d1);
        rho = o.strike *. o.expiry *. df_r *. Normal.cdf d2;
      }
  | Put ->
      {
        delta = -.df_q *. Normal.cdf (-.d1);
        gamma;
        vega;
        theta =
          decay
          +. (o.rate *. o.strike *. df_r *. Normal.cdf (-.d2))
          -. (o.dividend *. o.spot *. df_q *. Normal.cdf (-.d1));
        rho = -.o.strike *. o.expiry *. df_r *. Normal.cdf (-.d2);
      }
