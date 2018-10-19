
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
let rec str_id_list li =
	if (List.length li)==0 then ""	
	else match (List.hd li) with
	| A -> "A "^(str_id_list (List.tl li)) 
	| B -> "B "^(str_id_list (List.tl li)) 
	| C -> "C "^(str_id_list (List.tl li)) 
	| D -> "D "^(str_id_list (List.tl li)) 
	| E -> "E "^(str_id_list (List.tl li)) 

let rec str_int_list li = 
	if (List.length li)==0 then ""	
	else (string_of_int (List.hd li))^" "^(str_int_list (List.tl li))

(* debuggin : print *)

let print_id_list li =
	let _ = print_endline(str_id_list li) in ()

let print_int_list li =
	let _ = print_endline(str_int_list li) in ()



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

let rec id_to_int_list li =
	if (List.length li)==0 then []
	else let head = (List.hd li) in 
		[id_to_int head]@(id_to_int_list (List.tl li))

(* insert value to the list, keeping the order *)
let rec insert (li, value) =
	if (List.length li) == 0 then [value]
	else let head = (List.hd li) in
		if value>head then [head]@(insert (List.tl li, value))
		else if value=head then li
		else [value]@li

let insert_id (li, id) = 
	let temp = id_to_int_list li in
	let temp2 = insert(temp, (id_to_int id)) in
	int_to_id_list temp2

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

(* TODO *)
(* cond : condition list *)
(* evaluate initset, edgeset *)
let rec eval_condition_list cond =
	let rec eval_condition cond = 
		match cond with 
		| Items gifts -> (gift_to_int_list gifts, [])
		| Same id -> ([], [id_to_int id])
		| Common (c1, c2) ->
			let (init1, edge1) = eval_condition c1 in
			let (init2, edge2) = eval_condition c2 in
			(union(init1, init2), union(edge1, edge2))
		|Except (c, gl) -> eval_condition c in
	
	if (List.length cond)==0 then ([],[])
	else let (irec, erec) = (eval_condition_list (List.tl cond)) in
		let (i0, e0) = (eval_condition (List.hd cond)) in
		(union(i0, irec), union(e0, erec))


(* return initial sets, edges(graph), and invoke as functions *)
let rec initialize req = (* init, edge *)
	if (List.length req) = 0 
		then ((fun id -> raise NoID), 
				(fun id -> if id=A then []
				 			else if id=B then []
							else if id=C then []
							else if id=D then []
							else if id=E then []
							else raise NoID), 
				(fun id -> raise NoID)) 
	else let (id0, cond0) = (List.hd req) in
		(* recursion *)
		(* init, edge, invoke : ftn *)
		let (init, edge, invoke) = initialize (List.tl req) in
		(* and update the current information *)
		let (init_curr, edge_curr) = eval_condition_list cond0 in
		
		(* inner function : make edge function *)
		(* reverse the direction of edge_curr *)
		let rec build_edge (ftn, edges) = 
			if (List.length edges)==0 then ftn
			else let new_ftn = build_edge (ftn, (List.tl edges)) in 
			(fun id -> if id=(List.hd edges) then insert_id((new_ftn id), id0) 
			 			else new_ftn id) in

		((fun id -> if id=id0 then init_curr else init id),
		 (*(fun id -> if id=id0 then int_to_id_list(edge_curr) else edge id),*)
		 (build_edge (edge, int_to_id_list(edge_curr))),
		 (fun id -> if id=id0 then true else invoke id)) 



let req = [ 
  (A, [Common (Common (Same B, Same C), Common (Same D, Same E))]); 
  (B, [Common (Same C, Common (Same D, Except (Same E, [5]))); 
   	  Same A; Same C; Items [4; 2; 4]]); 
  (C, [Same D; Items[7;8]]); 
  (D, [Except (Same E, [1;2;3])]); 
  (E, [Items [1;2;3;4;5]]); ]
 
let (init, edge, invoke) = initialize req

let _ = print_int_list(init A)
let _ = print_int_list(init B)
let _ = print_int_list(init C)
let _ = print_int_list(init D)
let _ = print_int_list(init E)

let _ = print_id_list(edge A)
let _ = print_id_list(edge B)
let _ = print_id_list(edge C)
let _ = print_id_list(edge D)
let _ = print_id_list(edge E)


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
