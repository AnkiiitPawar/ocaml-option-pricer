(* Standard normal distribution. *)

let pdf x = exp (-0.5 *. x *. x) /. sqrt (2.0 *. Float.pi)

(* Phi(x) = erfc(-x / sqrt 2) / 2, accurate in both tails (no 1 - small cancellation). *)
let cdf x = 0.5 *. Float.erfc (-.x /. sqrt 2.0)
