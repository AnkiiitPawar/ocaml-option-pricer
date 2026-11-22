(* Contract and market description shared by every pricing method. *)

type kind = Call | Put

type t = {
  spot : float;  (* S: current price of the underlying *)
  strike : float;  (* K *)
  rate : float;  (* r: continuously compounded risk-free rate *)
  dividend : float;  (* q: continuous dividend yield *)
  vol : float;  (* sigma: annualised volatility *)
  expiry : float;  (* T: time to expiry in years *)
}

let make ?(dividend = 0.0) ~spot ~strike ~rate ~vol ~expiry () =
  if spot <= 0.0 || strike <= 0.0 || vol <= 0.0 || expiry <= 0.0 then
    invalid_arg "Contract.make: spot, strike, vol and expiry must be positive";
  { spot; strike; rate; dividend; vol; expiry }

let payoff kind ~strike s =
  match kind with Call -> Float.max (s -. strike) 0.0 | Put -> Float.max (strike -. s) 0.0

let kind_to_string = function Call -> "call" | Put -> "put"
