(**
   Title  : CS 496 A - HW Assignment 2
   Desc   : Encoding binary decision trees in OCaml using algebraic data types and simple operations on them
   Name   : Bobby Georgiou
   Date   : 02/23/2019
   Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
*)

type dTree =
  | Leaf of int
  | Node of char * dTree * dTree

let tLeft = Node('w', Node('x', Leaf(2), Leaf(5)), Leaf(8))
let tRight = Node('w', Node('x', Leaf(2), Leaf(5)), Node('y', Leaf(7), Leaf(5)))

(* [Helper] Returns the maximum value of a list *)
let my_max = function
  | [] -> 0
  | x::xs -> List.fold_left max x xs

(* Given a dTree, returns its height *)
let rec dTree_height : dTree -> int = fun t -> match t with
  | Leaf _ -> 0
  | Node(_, lt, rt) -> 1 + my_max([dTree_height lt; dTree_height rt])

(* Given a dTree, returns the total number of nodes and leaves *)
let rec dTree_size : dTree -> int = fun t -> match t with
  | Leaf _ -> 1
  | Node(_, lt, rt) -> 1 + dTree_size lt + dTree_size rt

(* Given a dTree, returns a list of all root-to-leaf paths *)
let rec dTree_paths : dTree -> int list list = fun t -> match t with
  | Leaf _ -> [[]]
  | Node(_, lt, rt) -> List.map (fun x -> 0 :: x) (dTree_paths lt) @ List.map (fun x -> 1 :: x) (dTree_paths rt)

(* Given a dTree, returns a boolean value representing if all leaves of the tree have the same depth *)
let rec dTree_is_perfect : dTree -> bool = fun t -> match t with
  | Leaf _ -> true
  | Node(_, lt, rt) -> (dTree_height lt = dTree_height rt) && dTree_is_perfect lt && dTree_is_perfect rt

(* Given a dTree, applies function f to each node and applies function g to each leaf *)
let rec dTree_map : (char -> char) -> (int -> int) -> dTree -> dTree = fun f g t -> match t with
  | Leaf n -> Leaf(g n)
  | Node(c, lt, rt) -> Node(f c, dTree_map f g lt, dTree_map f g rt)

(* Given a char list, builds a tree with node values of the characters and zero-valued leaves *)
let rec list_to_tree : char list -> dTree = fun l -> match l with
  | [] -> Leaf 0
  | c::cs -> Node(c, list_to_tree cs, list_to_tree cs)

(* [Helper] Iterates through a tree path and replaces leaf values where applicable *)
let rec replace_helper = fun t (lst, v) -> match t,lst with
  | Node(c, lt, rt), [] -> failwith("Path does not reach a leaf")
  | Leaf x, y::ys -> failwith("Leaf reached, but path is not exhausted")
  | Leaf x, [] -> Leaf v
  | Node(c, lt, rt), y::ys -> if y=0 then Node(c, replace_helper lt (ys, v), rt) else Node(c, lt, replace_helper rt (ys, v))

(* Given a dTree and boolean function, replaces the values of the tree leaves by the value indicated by the function *)
let rec replace_leaf_at = fun t f -> match f with
  | [] -> t
  | [(lst, n)] -> replace_helper t (lst, n)
  | ((lst, n)::zs) -> replace_leaf_at (replace_helper t (lst, n)) zs

(* Given a boolean function, builds a dTree *)
let rec bf_to_dTree = fun f -> match f with
  | (fst, snd) -> replace_leaf_at (list_to_tree fst) snd
