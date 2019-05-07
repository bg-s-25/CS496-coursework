(**
   Title  : CS 496 A - HW Assignment 1
   Name   : Bobby Georgiou
   Date   : 02/09/2019
   Pledge : "I pledge my honor that I have abided by the Stevens Honor System."
*)

(* ******************************************** *)
(** Basic functions on finite automata *)
(* ******************************************** *)

type symbol = char
type input = char list

type state = string

(* transition function *)
type tf = (state * symbol * state) list

(* initial state * transition function * end state *)
type fa = { states: state list; start:state; tf: tf; final: state list}


(* ******************************************** *)
(* Examples of automata *)
(* ******************************************** *)

let a = {states = ["q0";"q1";"q2"];
         start = "q0";
         tf = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")];
         final = ["q2"]}

let a2 = {states = ["q0";"q1";"q2";"q3";"q4"];
          start = "q0";
          tf = [("q0",'a',"q1"); ("q1",'b',"q1");
                ("q1",'c',"q2");  ("q3",'a',"q4")];
          final= ["q2"]}

let tf_of_a = [("q0",'a',"q1"); ("q1",'b',"q1"); ("q1",'c',"q2")]


(* ******************************************** *)
(* Helper functions *)
(* ******************************************** *)

let input_of_string s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []


(* ******************************************** *)
(* Simulating automata *)
(* ******************************************** *)

let member a l = List.mem a l

(* Apply transition function to return the resultant state given a current state and a symbol *)
let rec apply_transition_function = fun x st sym -> match x,st,sym with
  | [],_,_ -> None
  | b::bs,st2,sym2 -> (match b with 
                    | (st2,sym2,final) -> if st2=st && sym2=sym then Some final
                                            else apply_transition_function bs st sym)

(* [Helper] returns the resultant state once the input is exhausted by applying the transition function *)
let rec accept_helper fa state syms = match state,syms with
  | None,_ -> None
  | Some state,[] -> Some state
  | Some state,x::xs -> accept_helper fa (apply_transition_function fa.tf state x) xs

(* Determines whether the given input is accepted by an fa *)
let accept fa syms = 
  let x = (accept_helper fa (Some fa.start) syms) in match x with
    | Some state -> if (member state fa.final) then true else false
    | _ -> false

(* [Helper] returns true if a transition tuple is found that violates deterministic properties *)
let rec deterministic_helper = fun tf st sym result -> match tf with
  | [] -> false
  | x::xs -> (match x with
            | (a,b,c) -> if a=st && b=sym && c!=result then true else (deterministic_helper xs st sym result))

(* Checks whether the given fa is deterministic or not *)
let rec deterministic fa =
  let rec determ2 = fun tf -> match tf with
                    | [] -> true
                    | x::xs -> (match x with
                              | (a, b, c) -> if (deterministic_helper tf a b c) then false
                                else determ2 xs) in
  determ2 fa.tf

(* [Helper] checks if the final states are in the states list *)
let rec valid_final = fun fin sts -> match fin with
  | [] -> true
  | x::xs -> member x sts && valid_final xs sts

(* Checks for the validity of the given fa; it is valid if deterministic and the start/final states are in the states list *)
let valid fa = (member fa.start fa.states) && (valid_final fa.final fa.states) && (deterministic fa)

(* [Helper] looks through transitions list and build list of reachable states *)
let rec check_reachable = fun tf current lst -> match tf with
  | [] -> lst
  | x::xs -> (match x with
            | (first,_,second) -> if (member first lst)=false && first=current && first!=second then check_reachable xs second lst@[second]
                                    else check_reachable xs second lst)

(* Returns a list of states that are reachable from the start state in the given fa *)
let reachable fa = check_reachable fa.tf fa.start [fa.start]

(* Removes unreachable states from the definition of the given fa and returns a new fa *)
let remove_dead_states = fun fa -> let new_states = reachable fa in
    {states = new_states;
    start = fa.start;
    tf = List.find_all (fun (a,_,c) -> (member a new_states) && (member c new_states)) fa.tf;
    final = List.find_all (fun a -> member a new_states) fa.final;}
