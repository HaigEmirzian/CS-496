(* 
   Stub for HW2 
   Please rename to gt.ml
   Name: Haig Emirzian 
   Pledge: I pledge my honor that I have abided by the Stevens Honor System.
*)

type 'a gt = Node of 'a*('a gt) list

let mk_leaf (n:'a) : 'a gt =
  Node(n,[])
    
let t : int gt =
 Node (33,
       [Node (12,[]);
        Node (77, 
              [Node (37, 
                     [Node (14, [])]); 
               Node (48, []); 
               Node (103, [])])
       ])


(* height of tree *)
let rec height (t : 'a gt) : int =
  match t with
  | Node(_, []) -> 1
  | Node(_, x) -> 1 + List.fold_left max 0 (List.map height x) 
   
  
(* size of tree *)
  let rec size (t : 'a gt) : int =
  match t with
  | Node(_, []) -> 1
  | Node(_, x) -> 1 + List.fold_left (+) 0 (List.map size x) 


(* number of paths from root to leaves *)
let rec paths_to_leaves (Node(d,ch) : 'a gt) : int list list = 
  let rec iterator x y =
    match y with
    |[] -> []
    |h::t -> (List.map(fun path -> x :: path) (paths_to_leaves h)) @ (iterator (x + 1) t) in
    match (Node(d,ch)) with
    |Node(x, []) -> [[]]
    |Node(x, y) -> iterator 0 y
    
    
(* helper function for is_leaf_perfect to add everything in the list*)
let rec add = function
| [] -> 0
| h::t -> h + add t


(* perfect tree pretty much *)
let rec is_leaf_perfect (t : 'a gt) : bool =
  let length_p = List.map List.length @@ paths_to_leaves t in
  match length_p with
  | [] -> false
  | h::t -> if h = (add length_p)/h then true else false
      

(* preorder of the tree *)
let rec preorder (Node(d,ch) : 'a gt ) : int list =
  match Node(d, ch) with
  | Node(_, []) -> [d]  
  | Node(_, x) -> d::(List.flatten(List.map preorder ch))


(* mirrors the tree *)                     
let rec mirror (Node(d,ch) : 'a gt) : 'a gt =
  match Node(d, ch) with 
  | Node(_, []) -> Node(d, [])
  | Node(_, x) -> Node(d, List.rev(List.map mirror ch))


(* takes in a function to map *) 
let rec map (f : 'a -> 'b) (Node(d,ch) : 'a gt) : 'b gt =
  (Node(f d, List.map(map f) ch))


(* fold to reduce the tree *)  
let rec fold : ('a -> 'b list -> 'b) -> 'a gt -> 'b =
  fun f (Node(d,ch)) ->
    f d (List.map(fold f) ch)


(* sum function *)
let sum (t : 'a gt) : int =
  fold (fun i rs -> i + List.fold_left (fun i j -> i+j) 0 rs) t


(* checks to see if in list *)
let mem (t : 'a gt) (e : 'a) : bool = 
  fold (fun i rs -> i=e || List.exists (fun i -> i) rs) t


(* mirror but using fold *)
let rec mirror' (t : 'a gt) : 'a gt = 
  fold(fun i rs -> Node(i, List.rev rs)) t


(* checks to see the max number of children of one of the nodes *)
let rec degree (t : 'a gt) : int = 
  match t with
  | Node(_, []) -> 0
  | Node(_, ch) -> max(List.length ch) (List.fold_left max 0 (List.map degree ch))
