(* Find an element inside a list; returns true if it is found and false otherwise *)
let rec find = fun lst a -> match lst with
  | [] -> false
  | x::xs -> 
    if x=a then true
    else (find xs a)

(* Get sum of numbers in a list *)
let rec sum = fun lst -> match lst with
  | [] -> 0
  | x::xs -> x + sum xs

(* Fibonacci sequence *)
let rec fib = fun n -> match n with
  | 0 -> 0
  | 1 -> 1
  | x -> (fib x-1) + (fib x-2)
