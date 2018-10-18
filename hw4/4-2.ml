
exception NoID

type require = id * (cond list)
and cond
	= Items of gift list (* 선물들 *)
	| Same of id (* 어느 조카의 선물들 *)
	| Common of cond * cond (* 두조건에 공통된 선물들 *)
	| Except of cond * gift list (* 조건에서 어느 선물들은 빼고 *)
and gift = int (* 선물 번호 *)
and id = A | B | C | D | E (* 조카 이름 *)

type invoke = id * bool

type edge = id * (id list)

(* debuggin *)
let rec print_id li =
	if (List.length li)==0 then ""	
	else match (List.hd li) with
	| A -> "A "^(print_id (List.tl li)) 
	| B -> "B "^(print_id (List.tl li)) 
	| C -> "C "^(print_id (List.tl li)) 
	| D -> "D "^(print_id (List.tl li)) 
	| E -> "E "^(print_id (List.tl li)) 

let id_to_int id = 
	match id with
	| A -> 0
	| B -> 1
	| C -> 2
	| D -> 3
	| E -> 4

let int_to_id value =
	match value with
	| 0 -> A
	| 1 -> B
	| 2 -> C
	| 3 -> D
	| 4 -> E
	| _ -> raise NoID

let rec int_to_id_list li =
	if (List.length li)==0 then []
	else let head = (List.hd li) in 
		[int_to_id head]@(int_to_id_list (List.tl li))

(* insert value to the list, keeping the order *)
let rec insert (li, value) =
	if (List.length li) == 0 then [value]
	else let head = (List.hd li) in
		if value>head then [head]@(insert (List.tl li, value))
		else if value=head then li
		else [value]@li

(* compare two sorted lists *)
let rec compare (l1, l2) = 
	if(List.length l1) != (List.length l2) then false
	else if (List.length l1) == 0 then true
	else if (List.hd l1) != (List.hd l2) then false
	else compare((List.tl l1), (List.tl l2))

(* convert gift list to sorted int list *)
let rec gift_to_int_list gifts = 
	if (List.length gifts) = 0 then []
	else insert((gift_to_int_list (List.tl gifts)), (List.hd gifts))

(* union two sorted lists *)
let rec union (l1, l2) =
	if(List.length l2)==0 then l1
	else insert((union (l1, (List.tl l2)), (List.hd l2)))

(* TODO
let rec eval_condition cond =
	match cond with 
	| Items gifts 
*)

(* return initial sets, edges(graph), and invoke as functions *)
let rec initialize req = (* init, edge *)
	if (List.length req) = 0 
		then ((fun id -> []), (fun id -> []), (fun id -> false)) 
	else let (id0, cond0) = (List.hd req) in
		(* recursion *)
		let (init, edge, invoke) = initialize (List.tl req) in
		(* and update the current information *)
		

		((fun id -> if id=id0 then [id0] else init id),
		 (fun id -> if id=id0 then [id0] else edge id),
		 invoke) 



let req = [(A, []); (B, [])]

let (x, y, z) = initialize req 


(*
let x = [1; 4; 5]
let x = insert(x, 4)
let y = [2; 4; 5; 6]
let z = union(x, y)
*)
(*
let x = []
let x = insert (x, 3)
let x = insert (x, 5)
let x = insert (x, 1)
let x = insert (x, 6)
let x = insert (x, 4)
let x = insert (x, 2)
let x = insert (x, 0)

let y1 = [0;1;2;3;4;5;6]
let y2 = [0;1;3;3;4;5;6]
let y3 = [0;1;2;3;4;5;6;7]
let y4 = [0;1;2;3;4;5]
let y5 = [0;1;2;3;4;5;10]
let y6 = [1;1;2;3;4;5;6]

let _ = print_endline(string_of_bool (compare(x, y1)))
let _ = print_endline(string_of_bool (compare(x, y2)))
let _ = print_endline(string_of_bool (compare(x, y3)))
let _ = print_endline(string_of_bool (compare(x, y4)))
let _ = print_endline(string_of_bool (compare(x, y5)))
let _ = print_endline(string_of_bool (compare(x, y6)))
*)



(*let li = x A

let test = [A; B; C]
let _ = print_endline (print_id li)
*)
