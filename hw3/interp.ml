(**
   Title  : CS 496 A - HW Assignment 3
   Desc   : Extensions to LET language interpreter
   Name   : Bobby Georgiou
   Date   : 03/03/2019
   Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
*)

open Ast
open Ds

(* Helper function for Tl(e1) *)
let get_last = fun lst -> match lst with
  | [] -> failwith("List is empty")
  | x::xs -> List.tl lst

let rec eval (en:env) (e:expr):exp_val =
  match e with
  | Int n           -> NumVal n
  | Var x           -> lookup en x
  | Let(x, e1, e2)  ->
    let v1 = eval en e1  in
    eval (extend_env en x v1) e2
  | IsZero(e1)      ->
    let v1 = eval en e1  in
    let n1 = numVal_to_num v1 in
    BoolVal (n1 = 0)
  | ITE(e1, e2, e3) ->
    let v1 = eval en e1  in
    let b1 = boolVal_to_bool v1 in
    if b1 then eval en e2 else eval en e3
  | Sub(e1, e2)     ->
    let v1 = eval en e1 in
    let v2 = eval en e2  in
    NumVal ((numVal_to_num v1) - (numVal_to_num v2))
  | Add(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal ((numVal_to_num v1) + (numVal_to_num v2))
  | Div(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal ((numVal_to_num v1) / (numVal_to_num v2))
  | Mul(e1, e2)     -> 
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    NumVal ((numVal_to_num v1) * (numVal_to_num v2))
  | Abs(e1)         -> 
    let v1 = eval en e1 in
    if ((numVal_to_num v1) < 0) then NumVal (-1 * (numVal_to_num v1)) else NumVal (numVal_to_num v1)
  | Cons(e1, e2)    -> 
    let v1 = eval en e1 in
    let v2 = eval en e2 in
    ListVal (v1 :: (listVal_to_list v2))
  | Hd(e1)          -> 
    let v1 = eval en e1 in
    if is_listVal v1 then
      (match (listVal_to_list v1) with
        | [] -> failwith("List is empty")
        | x::xs -> x
      )
    else failwith("e1 is not a ListVal")
  | Tl(e1)          -> 
    let v1 = eval en e1 in
    ListVal (get_last (listVal_to_list v1))
  | Empty(e1)       -> 
    let v1 = eval en e1 in 
    (match v1 with
      | ListVal [] -> BoolVal true
      | TreeVal Empty -> BoolVal true
      | ListVal _ -> BoolVal false
      | TreeVal _ -> BoolVal false
      | _ -> failwith("e1 is not a ListVal or TreeVal")
    )
  | EmptyList       -> ListVal []
  | EmptyTree       -> TreeVal Empty
  | Node(e1,lt,rt)  -> 
    let v1 = eval en e1 in
    let v2 = eval en lt in
    let v3 = eval en rt in
    TreeVal(Node(v1, (treeVal_to_tree v2), (treeVal_to_tree v3)))
  | CaseT(target,emptycase,id_e,id_lt,id_rt,nodecase) -> 
    let v1 = eval en target in
    let t = treeVal_to_tree v1 in
    (match t with
      | Empty -> eval en emptycase (* extend environment only if target is a node *)
      | Node(ev,lt,rt) -> eval (extend_env (extend_env (extend_env en id_e ev) id_lt (TreeVal lt)) id_rt (TreeVal rt)) nodecase
    )


(***********************************************************************)
(* Everything above this is essentially the same as we saw in lecture. *)
(***********************************************************************)

(* Parse a string into an ast *)
let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

(* Interpret an expression *)
let interp (e:string):exp_val =
  e |> parse |> eval (empty_env ())
