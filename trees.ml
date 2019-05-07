(* CS 496 - In-class binary tree exercises *)

type 'a btree = Empty | Node of 'a * 'a btree * 'a btree

let t = Node(44, Node(30, Empty, Empty), Node(72, Node(54, Empty, Empty), Empty))

let rec find : 'a -> 'a btree -> bool = fun k t -> match t with
  | Empty -> false
  | Node(key, tL, tR) when k=key -> true
  | Node(key, tL, tR) -> if k>key then find k tR else find k tL

let rec maxt : 'a btree -> 'a = fun t -> match t with
  | Empty -> failwith "Not possible"
  | Node(key, _, Empty) -> key
  | Node(key, tL, tR) -> maxt tR

let rec add : 'a -> 'a btree -> 'a btree = fun k t -> match t with
  | Empty -> Node(k, Empty, Empty)
  | Node(key, tL, tR) when k=key -> t
  | Node(key, tL, tR) -> if k<key then Node(key, add k tL, tR) else Node(key, tL, add k tR)

(* delete returns None if key not in tree *)
let rec delete : 'a -> 'a btree -> 'a btree = fun k t -> match t with
  | Empty -> failwith "Key not in tree"
  | Node(key, tL, Empty) when k=key -> tL
  | Node(key, Empty, tR) when k=key -> tR
  | Node(key, tL, tR) when k=key -> let m = max tL in Node(m, delete m tL, tR)
  | Node(key, tL, tR) -> if k<key then Node(key, delete k tL, tR) else Node(key, tL, delete k tR)
