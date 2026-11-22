(* Monte Carlo under geometric Brownian motion with antithetic variates.

   S_T = S exp((r - q - sigma^2/2) T + sigma sqrt(T) Z). Each normal draw Z is
   used twice, as +Z and -Z; averaging the pair cancels much of the noise. *)

open Contract

let gaussian st =
  (* Box-Muller transform; 1 - U keeps the log argument in (0, 1]. *)
  let u1 = 1.0 -. Random.State.float st 1.0 in
  let u2 = Random.State.float st 1.0 in
  sqrt (-2.0 *. log u1) *. cos (2.0 *. Float.pi *. u2)

(* Returns (price, standard error). *)
let price ?(paths = 200_000) ?(seed = 42) kind o =
  let st = Random.State.make [| seed |] in
  let drift = (o.rate -. o.dividend -. (0.5 *. o.vol *. o.vol)) *. o.expiry in
  let diffusion = o.vol *. sqrt o.expiry in
  let disc = exp (-.o.rate *. o.expiry) in
  let pairs = max 2 (paths / 2) in
  let sum = ref 0.0 and sum_sq = ref 0.0 in
  for _ = 1 to pairs do
    let z = gaussian st in
    let up = payoff kind ~strike:o.strike (o.spot *. exp (drift +. (diffusion *. z))) in
    let down = payoff kind ~strike:o.strike (o.spot *. exp (drift -. (diffusion *. z))) in
    let x = 0.5 *. (up +. down) *. disc in
    sum := !sum +. x;
    sum_sq := !sum_sq +. (x *. x)
  done;
  let n = float_of_int pairs in
  let mean = !sum /. n in
  let variance = ((!sum_sq /. n) -. (mean *. mean)) *. n /. (n -. 1.0) in
  (mean, sqrt (variance /. n))
