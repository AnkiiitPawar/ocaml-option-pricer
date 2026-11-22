(* Cox-Ross-Rubinstein binomial tree, European or American exercise.

   Each step the price moves up by u = exp(sigma sqrt dt) or down by d = 1/u.
   The risk-neutral up-probability p = (exp((r - q) dt) - d) / (u - d) makes the
   discounted price a martingale. Values are rolled back from expiry; an
   American option takes max(continuation, exercise now) at every node. *)

open Contract

type exercise = European | American

let price ?(steps = 500) exercise kind o =
  if steps < 1 then invalid_arg "Binomial.price: steps must be >= 1";
  let dt = o.expiry /. float_of_int steps in
  let u = exp (o.vol *. sqrt dt) in
  let d = 1.0 /. u in
  let p = (exp ((o.rate -. o.dividend) *. dt) -. d) /. (u -. d) in
  if p <= 0.0 || p >= 1.0 then invalid_arg "Binomial.price: too few steps for these parameters";
  let disc = exp (-.o.rate *. dt) in
  let node i j = o.spot *. (u ** float_of_int j) *. (d ** float_of_int (i - j)) in
  let values = Array.init (steps + 1) (fun j -> payoff kind ~strike:o.strike (node steps j)) in
  for i = steps - 1 downto 0 do
    for j = 0 to i do
      let continuation = disc *. ((p *. values.(j + 1)) +. ((1.0 -. p) *. values.(j))) in
      values.(j) <-
        (match exercise with
        | European -> continuation
        | American -> Float.max continuation (payoff kind ~strike:o.strike (node i j)))
    done
  done;
  values.(0)
