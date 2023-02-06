(*Homework #1*)
(* I pledge my honor that I have abided by the Stevens Honor System. *)
(* Haig Emirzian *)

type program = int list

let letter_e =  [0;2;2;3;3;5;5;4;3;5;4;3;3;5;5;1]


(*
0 or 1: leave alone
2 or 3: add 2
4 or 5: subtract 2 *)  
let rec mirror_image (letter_e : int list) : int list = 
  match letter_e with
  | [] -> []
  | 2::t -> 4::(mirror_image t)
  | 3::t -> 5::(mirror_image t)
  | 4::t -> 2::(mirror_image t)
  | 5::t -> 3::(mirror_image t)
  | x::t -> x::(mirror_image t)
  

(*
0 or 1: leave alone
2: add 3
3 and 5: subtract 1
4: add 1 *)
let rec rotate_90_letter (letter_e: int list) : int list = 
  match letter_e with
  | [] -> []
  | 2::t -> 3::(rotate_90_letter t)
  | 3::t -> 4::(rotate_90_letter t)
  | 4::t -> 5::(rotate_90_letter t)
  | 5::t -> 2::(rotate_90_letter t)
  | x::t -> x::(rotate_90_letter t) 


(*
0 or 1: leave alone
2, 3, or 4: add 1
5: subtract 3 *)
let rec rotate_90_word (letter_e : int list list) : int list list =
  match letter_e with
  | [] -> []
  | x::t -> (rotate_90_letter x)::(rotate_90_word t)
       
                                  
(* repeats the input a certain amount of times *) 
let rec repeat (numberOfCopies : int) (input : 'a) : 'a list = 
  match numberOfCopies with
  | 0 -> []
  | _ -> input::(repeat (numberOfCopies - 1) input)
  

(* returns a program that draws the same things as p only enlarged n-fold *)                
(* uses map *) 
let pantograph (n : int) (p : int list) : int list =
  let l = (List.map (fun x -> if x > 1 then repeat n x else [x]) p) in
  List.fold_left (fun x y -> x @ y) [] l
      

(* does not use map *)
let rec pantograph_nm (n : int) (p : int list) : int list =
  match p with
  | [] -> []
  | 0::t -> 0::pantograph_nm n t
  | 1::t -> 1::pantograph_nm n t
  | x::t -> repeat n x @ (pantograph_nm n t) 
  

(* uses fold *)
let pantograph_f (n : int) (p : int list) : int list = 
  let l = (List.map (fun x -> if x > 1 then repeat n x else [x]) p) in
  List.fold_left (fun x y -> x @ y) [] l

(*
2: North (a, b+1)
3: East  (a+1, b)
4: South (a, b-1)
5: West  (a-1, b) *)
(* takes in directions and outputs coordinates*)
let rec coverage ((x,y) : int * int) (letter_e : int list) :  (int * int) list = 
  match letter_e with
  | [] -> (x,y)::[]
  | 2::t -> (x,y)::(coverage (x,y+1) t)
  | 3::t -> (x,y)::(coverage (x+1,y) t)
  | 4::t -> (x,y)::(coverage (x,y-1) t)
  | 5::t -> (x,y)::(coverage (x-1,y) t)
  | _::t -> (x,y)::(coverage (x,y) t)


(* compress helper function that helps count the consecutive duplicates *)
let rec count (e: int) (p : int list) : int*(int list) =
  match p with
  | [] -> (0,[])
  | h::t -> if (h=e) then let (c,new_l) = (count e t) in (c+1,new_l)
      else (0,p)
  

(* counts the number of adjacent copies *)
(* x,numberOfTimes *)
let rec compress (letter_e : int list) : ( int * int ) list = 
  match letter_e with
  | [] -> []
  | h::t -> let (c,new_l) = count h letter_e in
      (h, c)::(compress new_l)
    

(* uncompresses list *)
(* without map *)
let rec uncompress (comp : (int*int) list) : int list = 
  match comp with
  | [] -> []
  | (n, amt)::t -> (repeat amt n) @ (uncompress t)


(* with map *)
let rec uncompress_m (comp : (int*int) list) : int list =
  let l = (List.map (fun (x,n) -> repeat n x) comp) in
  List.fold_left (fun x y -> x @ y) [] l
    

(* with fold *)
let rec uncompress_f (comp : (int*int) list) : int list =
  let l = (List.map (fun (x,n) -> repeat n x) comp) in
  List.fold_left (fun x y -> x @ y) [] l


(* carries current state of the pen *)
let rec optimize_helper (list : program) (position : int) : program =
  match list with
  | [] -> []
  | 0::t -> if position=0 then (optimize_helper t 0) else 0::(optimize_helper t 0)
  | 1::t -> if position=1 then (optimize_helper t 1) else 1::(optimize_helper t 1)
  | x::t -> x::optimize_helper t position


(* optimize *)
let rec optimize (list : program) : program = 
  optimize_helper list 1