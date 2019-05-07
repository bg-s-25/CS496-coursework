(* MAP with integer list *)

let rec succl = int list -> int list = function 
  | [] -> []
  | x::xs -> (x+1) :: succl xs

let rec map_int = f = (int list -> int list) -> int list -> int list -> match xs with
  | [] -> []
  | x::xs -> f x :: map f xs
