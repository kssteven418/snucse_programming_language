
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


(*  print functions for debugging *)

let str_id id = 
	match id with
	| A -> "A"
	| B -> "B"
	| C -> "C"
	| D -> "D"
	| E -> "E"

let rec str_id_list li =
	if (List.length li)==0 then ""	
	else (str_id (List.hd li))^" "^(str_id_list (List.tl li)) 

let print_id_list li =
	let _ = print_endline(str_id_list li) in ()

let rec str_int_list li = 
	if (List.length li)==0 then ""	
	else (string_of_int (List.hd li))^" "^(str_int_list (List.tl li))

let print_int_list li =
	let _ = print_endline(str_int_list li) in ()

let str_result li = 
	let rec content li = 
		if (List.length li)==0 then ""
		else let (id, temp) = List.hd li in
			"("^(str_id id)^"["^(str_int_list temp)^"]"^") "^ (content (List.tl li)) in
	"["^(content li)^"]"

let rec print_result li =
	let _ = print_endline(str_result li) in ()

let print_set set =
	let _ = Printf.printf "A : " in
	let _ = print_int_list (set A) in
	let _ = Printf.printf "B : " in
	let _ = print_int_list (set B) in
	let _ = Printf.printf "C : " in
	let _ = print_int_list (set C) in
	let _ = Printf.printf "D : " in
	let _ = print_int_list (set D) in
	let _ = Printf.printf "E : " in
	let _ = print_int_list (set E) in
	let _ = print_endline("") in ()

let print_to_invoke invoke =
	let _ = Printf.printf("to invoke : ") in
	let print invoke id = 
		if invoke id then (str_id_list [id])^" " else "" in
	let temp = (print invoke A)^(print invoke B)^(print invoke C)
				^(print invoke D)^(print invoke E) in
	let _ = print_endline temp in ()

(* end of print functions *)

(* id-int conversion fuinctions *)
(* for set operations and sorting *)

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


(* set operations *)

(* insert value to the list, keeping the order *)
let rec insert (li, value) =
	if (List.length li) == 0 then [value]
	else let head = (List.hd li) in
		if value>head then [head]@(insert (List.tl li, value))
		else if value=head then li (* no duplication *)
		else [value]@li

(* insert id to the id list, keeping the order, no duplication allowed *)
(* by traslating into int list *)
let insert_id (li, id) = 
	let temp = id_to_int_list li in
	let temp2 = insert(temp, (id_to_int id)) in
	int_to_id_list temp2

(* compare two sorted lists *)
let rec compare (l1, l2) = 
	if(List.length l1) != (List.length l2) then false (* length mismatch *)
	else if (List.length l1) == 0 then true (* both are empty list *)
	else if (List.hd l1) != (List.hd l2) then false (* first elmt different *)
	else compare((List.tl l1), (List.tl l2)) (* first elmt same, then compare the tails *)

(* convert gift list to sorted int list *)
(* sort and remove redundancies *)
let rec gift_to_int_list gifts = 
	if (List.length gifts) = 0 then []
	(* this is done by inserting gift into the list one at a time *)
	(* use the insert function *)
	else insert((gift_to_int_list (List.tl gifts)), (List.hd gifts))

(* union two sorted lists *)
let rec union (l1, l2) =
	if(List.length l2)==0 then l1
	(* insert l2.head to the union of l1 and l2.tail *)
	else insert((union (l1, (List.tl l2)), (List.hd l2)))

(* intersection of two sorted list *)
let rec intersection(l1, l2) = 
	if(List.length l1) == 0 then []
	else if (List.length l2) == 0 then []
	else let h1 = (List.hd l1) in
		let h2 = (List.hd l2) in
		if (h1=h2) then insert(intersection((List.tl l1), (List.tl l2)), h1)
		else if (h1<h2) then intersection((List.tl l1), l2)
		else intersection(l1, (List.tl l2))

let rec except(l1, l2) =
	if(List.length l1) == 0 then [] 
	else if (List.length l2) == 0 then gift_to_int_list l1 (* nothing to except *)
	else let h1 = (List.hd l1) in
		let h2 = (List.hd l2) in
		if (h1=h2) then except((List.tl l1), l2)
		else if (h1>h2) then except(l1, (List.tl l2))
		else insert(except((List.tl l1), l2) ,h1)


(**************** MAIN PROCEDURES ******************)

(* cond : condition list *)
(* initiate edge set *)
let rec init_condition_list cond =
	(* inner fuction to evaluate one condition *)
	let rec init_condition cond = 
		match cond with 
		| Items gifts -> []
		| Same id -> [id_to_int id]
		| Common (c1, c2) ->
			let edge1 = init_condition c1 in
			let edge2 = init_condition c2 in
			union(edge1, edge2)
		| Except (c, gl) -> init_condition c in

	(* then union all conditions in the list *) 	
	if (List.length cond)==0 then []
	else let erec = (init_condition_list (List.tl cond)) in
		let  e0 = (init_condition (List.hd cond)) in
		union(e0, erec)


(* return initial sets, edges(graph), and invoke as functions *)
(* req is a list of (ID, conditions list) *)
let rec initialize req = 
	if (List.length req) = 0 
		then ((fun id -> if id=A then [] (* initial set *)
				 			else if id=B then []
							else if id=C then []
							else if id=D then []
							else if id=E then []
							else raise NoID), 
				(fun id -> if id=A then [] (* graph edge *)
				 			else if id=B then []
							else if id=C then []
							else if id=D then []
							else if id=E then []
							else raise NoID), 
				(fun id -> if id=A then true (* invoke *)
				 			else if id=B then true 
							else if id=C then true
							else if id=D then true
							else if id=E then true
							else raise NoID))
	else let (id0, cond0) = (List.hd req) in
		(* recursion *)
		(* init, edge, invoke : functions *)
		let (init, edge, invoke) = initialize (List.tl req) in
		(* and update the current information *)
		let edge_curr = init_condition_list cond0 in
		
		(* inner function : make edge function *)
		(* reverse the direction of edge_curr *)
		let rec build_edge (ftn, edges) = 
			if (List.length edges)==0 then ftn
			else let new_ftn = build_edge (ftn, (List.tl edges)) in 
			(*  edge_curr : id0 -> edges *)
			(* need to reverse : edges -> id0 *) 
			(fun id -> if id=(List.hd edges) then insert_id((new_ftn id), id0) 
			 			else new_ftn id) in

		((fun id -> if id=id0 then [] else init id), (* set initial set as an empty set *)
		 (build_edge (edge, int_to_id_list(edge_curr))), (* build edge function  *)
		 (fun id -> if id=id0 then true else invoke id)) (* set initial invoke as true *) 


(* evaluate condition list based on set of other ids *)
(* set is current gift set for all ids *)
let rec eval_condition_list (cond, set) =
	(* inner fuction to evaluate one condition *)
	let rec eval_condition (cond, set)  =
		match cond with
		| Items gifts -> gift_to_int_list gifts
		| Same id -> set id
		| Common (c1, c2) ->
			let s1 = eval_condition (c1, set) in
			let s2 = eval_condition (c2, set) in
			intersection(s1, s2)
		| Except (c, gl) ->
			let s1 = eval_condition (c, set) in
			let s2 = gift_to_int_list gl in (* sort and remove duplicate *)
			except(s1, s2) in

	(* then union all conditions in the list *) 	
	if (List.length cond)==0 then []
	else let s_rec = (eval_condition_list ((List.tl cond), set)) in
		let s_curr = eval_condition((List.hd cond), set) in
		union(s_rec, s_curr)

	
(* first, calculate request of the id *)
(* modify and return new set and invoke *) 
let eval_new_set (cond, id, set, edge, invoke) =
	let old_set = set id in (* set of the id before modification *)
	(* disable invoke of the id *)
	let invoke_disable_id = (fun x -> if x=id then false else invoke x) in
	let new_set = eval_condition_list(cond, set) in


	(* if new set equals old set, then no change, do nothing *)
	(* only return new invoke function *)
	if compare(old_set, new_set) then (set, invoke_disable_id)

	(* else, update set and invoke *)
	(* update the set *)
	else let set_update = (fun x -> if x=id then new_set else set x) in 
		let ids_to_invoke = edge id in
		(* update the invoke *)
		(* by setting all invoke ids of 'ids to invoke' as true *) 
		let rec set_invoke (invoke, ids) =
			if (List.length ids) = 0 then invoke
			else let new_invoke = set_invoke (invoke, (List.tl ids)) in
			(fun x -> if x=(List.hd ids) then true else (new_invoke x)) in
		
		let invoke_update = set_invoke(invoke, ids_to_invoke) in
		(set_update, invoke_update)


(* get id whose invoke entry is true *)
let id_to_invoke invoke =
	if invoke A then (true, A)
	else if invoke B then (true, B)
	else if invoke C then (true, C)
	else if invoke D then (true, D)
	else if invoke E then (true, E)
	else (false, A) 

let shoppingList req =
	(* make request list into request function *)
	let rec req_ftn_build req =
		if (List.length req)==0 then 
			(fun x -> if x=A then [] 
			 		else if x=B then []
					else if x=C then []
					else if x=D then []
					else if x=E then []
					else raise NoID)
		else let (id, cond) = (List.hd req) in
			let ftn_rec = req_ftn_build (List.tl req) in
			(fun x -> if x=id then ((ftn_rec x)@cond) else ftn_rec x) in
	
	let req_ftn = req_ftn_build req in
	
	(* initiation phase *)
	let (set, edge, invoke) = initialize req in
	(* inner function to ennumerate *)
	let rec ennumerate (set, edge, invoke)=
		(* find id to invoke *)
		let (go, id) = id_to_invoke invoke in
		if not go then set (* all done, return the input set as the final result *)
		else let cond = req_ftn id in (* condition list of the id *) 
			let (set_update, invoke_update) 
				= eval_new_set(cond, id, set, edge, invoke) in
			(*
			let _ = print_endline("selected id : "^(str_id_list [id])) in
			let _ = print_to_invoke invoke_update in
			let _ = print_set set_update in 
			*)
			ennumerate (set_update, edge, invoke_update) in
		
	let final_set = ennumerate(set, edge, invoke) in	
	
	[(A, final_set A);(B, final_set B);(C, final_set C);(D, final_set D);(E, final_set E)]




(* DEBUGGING 3 *)
(*
let test (c, a, num) = 
	let result = (shoppingList c) in
	let _ = if (result = a) 
		then print_endline ((string_of_int num) ^ " accepted ")
		else print_endline ((string_of_int num)^" failed\n"
							^(str_result a)^"\n"^(str_result result)) in ()

let check1 = [(A, [Items[1;2;3]]); (B, [Same C; Same D]); (C, [Except(Same A, [2])]); (D, [Except(Common(Same A, Same B),[3])]); (E, [])]
let check2 = [(A, [Items[1;4;3]]);(B, [Except(Same A, [4;3;1]);Same C]);(C, [Same B;Same D]);(D, []);(E, [])]
let check3 = [(A, [Items[1;4;3]]);(B, [Except(Same A, [4;3;1]);Same C]);(C, [Same B;Same D]);(D, [Items[7]]);(E, [])]
let check4 = [(A, [Same B]);(B, [Same C]);(C, [Same A]);(D,[Same A;Same E]);(E,[Same D])]
let check5 = [(A, [Items[1;2;3];Same D]);
			  (B, [Same A; Items[5]]);
			  (C, [Common(Common(Except(Same E, [1;2]),Same A),Same B)]);
			  (D, [Except(Same A, [3]);Same C]);
			  (E, [Items[1;2;3;4];Common(Same B, Same C)])]

let check6 = [(A, [Items[3;2;1];Same D]);
			  (B, [Except(Same C, [1]);Except(Same A, [1])]);
			  (C, [Common(Same A, Same D)]);
			  (D, [Items[4;5];Common(Same A, Same B)]);
			  (E, [Common(Same A, Same B);Same C])]
let check7 = [(A, [Except(Same E, [1;3])]);
			  (B, [Items[1;5;6];Common(Same A, Same C)]);
			  (C, [Items[1;2;3;4]]);
			  (D, [Common(Same A, Except(Same C, [3;4]))]);
			  (E, [Common(Same A, Same B);Common(Same D, Same C)])]
let check8 = [(A, [Common(Same B, Same C);Same E]);
			  (B, [Items[1;5;6];Common(Same A, Same C)]);
			  (C, [Items[1;2;3;4]]);
			  (D, [Common(Same A, Except(Same C, [3;4]))]);
			  (E, [Common(Same C, Common(Same A, Same B));Common(Same D, Same C)])]

let answer1 = [(A,[1;2;3]);(B,[1;3]);(C,[1;3]);(D,[1]);(E,[])]  
let answer2 = [(A,[1;3;4]);(B,[]);(C,[]);(D,[]);(E,[])]  
let answer3 = [(A,[1;3;4]);(B,[7]);(C,[7]);(D,[7]);(E,[])]  
let answer4 = [(A,[]);(B,[]);(C,[]);(D,[]);(E,[])]
let answer5 = [(A,[1;2;3]);(B,[1;2;3;5]);(C,[3]);(D,[1;2;3]);(E,[1;2;3;4])]
let answer6 = [(A,[1;2;3;4;5]);(B,[2;3;4;5]);(C,[2;3;4;5]);(D,[2;3;4;5]);(E,[2;3;4;5])]
let answer7 = [(A,[]);(B,[1;5;6]);(C,[1;2;3;4]);(D,[]);(E,[])]
let answer8 = [(A,[1]);(B,[1;5;6]);(C,[1;2;3;4]);(D,[1]);(E,[1])]

let _ = test (check1, answer1, 1)
let _ = test (check2, answer2, 2)
let _ = test (check3, answer3, 3)
let _ = test (check4, answer4, 4)
let _ = test (check5, answer5, 5)
let _ = test (check6, answer6, 6)
let _ = test (check7, answer7, 7)
let _ = test (check8, answer8, 8)
let _ = print_result(shoppingList(check8))
*)


(* DEBUGGING 2 *)
(*
let idtest (h) = 
    match h with 
	(id, gift) -> 
	match id with 
	| A -> "A" 
    | B -> "B" 
    | C -> "C" 
    | D -> "D" 
    | E -> "E" 

let gifttest (h) = 
    match h with 
    | (id, gift) -> gift 
	let rec printlisttest (gift) = 
    match gift with 
    | [] -> "]" 
    | h::t -> (string_of_int h) ^ printlisttest(t) 

let rec printfinaltest (result) = 
	    match result with 
	    | [] -> () 
	    | h::t -> print_string ("(" ^ idtest(h) ^ ",[" ^ printlisttest(gifttest(h)) ^");") 
		    
let check1 = [(A, Items[1;2]::[]); (B, []); (C, []); (D, []); (E, [])] 
let check2 = [(A, Items[1;2]::Items[3;4]::[]); (B, []); (C, []); (D, []); (E, [])] 
let check3 = [(A,Items[1;2]::Items[1;4]::[]);(C,Items[3;4]::[]); (B, []); (D, []); (E, [])] 
let check4 = [(A,Items[1;2]::Items[1;4]::[]);(B, Same A::[]);(C,Items[3;4]::[]); (D, []); (E, [])] 
let check5 = [(A,Items[1;2]::Items[1;4]::[]);(B, Same C::[]);(C,Items[3;4]::[]); (D, []); (E, [])] 
let check6 = [(A,Same B::[]);(B, Same C::[]);(C, Same D::[]);(D, Same E::[]);(E, Same A::[])] 
let check7 = [(A, Items [1;2;3]::[]); (B,Same A::Items [4]::[]); (C, []); (D, []); (E, [Same D])] 

let check8 = [(A, [Items[1;2]]); (B, [Same A]); (C, []); (D, []); (E, [])] 
let check9 = [(A, [Items[1;2]]); (B, [Same C]);(C, [Items[2;3]]); (D, []); (E, [])] 
let check10 = [(A, [Items[1;2]]); (B, [Items[4];Same C]);(C, [Items[2;3]]); (D, []); (E, [])] 
let check11 = [(A, [Items[1;2]]); (B, [Same C;Items[4]]);(C, [Items[2;3]]); (D, []); (E, [])] 
let check12 = [(A, [Items[1;2]; Same C]); (B, [Same A]);(C, [Items[2;3]]); (D, []); (E, [])] 
let check13 = [(A, [Except (Items[1;2;3], [1;2])]); (B, []); (C, []); (D, []); (E, [])] 
let check14 = [(A, [Items[1;2;3]]);(B, [Except (Same A, [1])]); (C, []); (D, []); (E, [])] 
let check15 = [(A, [Common (Items[1;2], Items[2;3])]); (B, []); (C, []); (D, []); (E, [])] 
let check16 = [(A, [Items [5;6]; Common(Items [1;2], Items [2;3])]); (B, []); (C, []); (D, []); (E, [])] 
let check17 = [(A, [Items[1;2;3;4;5]]);(B,[Same D]); (C,[Common (Common (Except (Items [1;2;3;4;5], [3;4]), Same B), Same A)]);(D, [Items [1;3;5;7;9]]); (E, [])] 

let x = shoppingList(check17) 
let _ = print_result x
let _ = print_result ([(A,[1;2;3;4;5]);(B,[1;3;5;7;9]);(C,[1;5]);(D,[1;3;5;7;9]);(E,[])])
let _ = 
if (shoppingList (check1) = [(A,[1;2]);(B,[]);(C,[]);(D,[]);(E,[])])          then print_endline("1") else printfinaltest(shoppingList(check1)); 
if (shoppingList (check2) = [(A,[1;2;3;4]);(B,[]);(C,[]);(D,[]);(E,[])])       then print_endline("2") else printfinaltest(shoppingList(check2)); 
if (shoppingList (check3) = [(A,[1;2;4]);(B,[]);(C,[3;4]);(D,[]);(E,[])])       then print_endline("3") else printfinaltest(shoppingList(check3)); 
if (shoppingList (check4) = [(A,[1;2;4]);(B,[1;2;4]);(C,[3;4]);(D,[]);(E,[])])    then print_endline("4") else printfinaltest(shoppingList(check4)); 
if (shoppingList (check5) = [(A,[1;2;4]);(B,[3;4]);(C,[3;4]);(D,[]);(E,[])])    then print_endline("5") else printfinaltest(shoppingList(check5)); 
if (shoppingList (check6) = [(A,[]);(B,[]);(C,[]);(D,[]);(E,[])])             then print_endline("6") else printfinaltest(shoppingList(check6)); 
if (shoppingList (check7) = [(A,[1;2;3]);(B,[1;2;3;4]);(C,[]);(D,[]);(E,[])])   then print_endline("7") else printfinaltest(shoppingList(check7)); 
if (shoppingList (check8) = [(A,[1;2]);(B,[1;2]);(C,[]);(D,[]);(E,[])])       then print_endline("8") else printfinaltest(shoppingList(check8)); 
if (shoppingList (check9) = [(A,[1;2]);(B,[2;3]);(C,[2;3]);(D,[]);(E,[])])       then print_endline("9") else printfinaltest(shoppingList(check9)); 
if (shoppingList (check10) = [(A,[1;2]);(B,[2;3;4]);(C,[2;3]);(D,[]);(E,[])])    then print_endline("10") else printfinaltest(shoppingList(check10)); 
if (shoppingList (check11) = [(A,[1;2]);(B,[2;3;4]);(C,[2;3]);(D,[]);(E,[])])    then print_endline("11") else printfinaltest(shoppingList(check11)); 
if (shoppingList (check12) = [(A,[1;2;3]);(B,[1;2;3]);(C,[2;3]);(D,[]);(E,[])]) then print_endline("12") else printfinaltest(shoppingList(check12)); 
if (shoppingList (check13) = [(A,[3]);(B,[]);(C,[]);(D,[]);(E,[])])          then print_endline("13") else printfinaltest(shoppingList(check13)); 
if (shoppingList (check14) = [(A,[1;2;3]);(B,[2;3]);(C,[]);(D,[]);(E,[])])       then print_endline("14") else printfinaltest(shoppingList(check14)); 
if (shoppingList (check15) = [(A,[2]);(B,[]);(C,[]);(D,[]);(E,[])])          then print_endline("15") else printfinaltest(shoppingList(check15)); 
if (shoppingList (check16) = [(A,[2;5;6]);(B,[]);(C,[]);(D,[]);(E,[])])       then print_endline("16") else printfinaltest(shoppingList(check16)); 
if (shoppingList (check17) = [(A,[1;2;3;4;5]);(B,[1;3;5;7;9]);(C,[1;5]);(D,[1;3;5;7;9]);(E,[])]) then print_endline("17") else printfinaltest(shoppingList(check17)); 
*)

(* DEBUGGING *)
(*
let _ = 
let emptyL = [(A, []); (B, []); (C, []); (D, []); (E, [])] in 

assert ((shoppingList [ 
			(A, []); (B, []); (C, []); (D, []); (E, []); 
			]) = emptyL); 
print_endline "1"; 

assert ((shoppingList [ 
			(A, [Same B]); (B, [Same C]); (C, [Same D]); (D, [Same E]); (E, [Same A]); 
			]) = emptyL); 
print_endline "2"; 

assert ((shoppingList [ 
			(A, [Items [1;2;3]]); (B, [Items [2;3;4]]); 
			(C, [Items [3;4;1]]); (D, [Items [4;1;2]]); 
			(E, [Items [1;2;3;1;2;3]]); 
			]) = [(A, [1; 2; 3]); (B, [2; 3; 4]); (C, [1; 3; 4]); (D, [1; 2; 4]); (E, [1; 2; 3])]); 
print_endline "3"; 

assert ((shoppingList [ 
			(A, [Items [1;2;3]]); 
			(B, [Same A]); 
			(C, [Same A; Items[1;2]]); 
			(D, [Same A; Items[4]]); 
			(E, [Same D]); 
			]) = [(A, [1; 2; 3]); (B, [1; 2; 3]); (C, [1; 2; 3]); (D, [1; 2; 3; 4]); (E, [1; 2; 3; 4])]); 
print_endline "4"; 

assert ((shoppingList [ 
			(A, [Common (Items [1;2;3], Items [2;1;3])]); 
			(B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])]); 
			(C, [Common (Items [1;2;3], Items [4;5;6])]); 
			(D, [Common (Items [3;2;1], Items [1])]); 
			(E, [Common (Items [1;2;3], Items [])]); 
			]) = [(A, [1; 2; 3]); (B, [1; 2; 3]); (C, []); (D, [1]); (E, [])]); 
print_endline "5"; 

assert ((shoppingList [ 
			(B, [Common (Items [2;1;3], Items [5;6;1;4;2;3])]); 
			(E, [Common (Items [], Items [])]); 
			(D, [Common (Items [1], Items [1])]); 
			]) = [(A, []); (B, [1; 2; 3]); (C, []); (D, [1]); (E, [])]); 
print_endline "6"; 

assert ((shoppingList [ 
			(A, [Except (Items [3;2;1], [3;2;1])]); 
			(B, [Except (Items [2;1;3], [])]); 
			(C, [Except (Items [2;1;3], [1;2;3;4;5;6])]); 
			(D, [Except (Items [], [2;1;3])]); 
			(E, [Except (Items [], [])]); 
			]) = [(A, []); (B, [1; 2; 3]); (C, []); (D, []); (E, [])]); 
print_endline "7"; 

assert ((shoppingList [ 
			(A, [Common (Common (Same B, Same C), Common (Same D, Same E))]); 
			(B, [Common (Same C, Common (Same D, Except (Same E, [5])))]); 
			(C, [Same D; Items[7;8]]); 
			(D, [Except (Same E, [1;2;3])]); 
			(E, [Items [1;2;3;4;5]]); 
			]) = [(A, [4]); (B, [4]); (C, [4; 5; 7; 8]); (D, [4; 5]); (E, [1; 2; 3; 4; 5])]); 
print_endline "8"; 

assert ((shoppingList [ 
			(A, [Same B; Same C]); 
			(B, [Except (Same C, [1;2;3]); Same D]); 
			(C, [Items [1;2;3]; Items [3;4;5]; Common (Same A, Items [6;7])]); 
			(D, [Same E]); 
			(E, [Same D; Items[6;8]]); 
			]) = [(A, [1; 2; 3; 4; 5; 6; 8]); (B, [4; 5; 6; 8]); (C, [1; 2; 3; 4; 5; 6]); (D, [6; 8]); (E, [6; 8])]); 
print_endline "9"; 

assert ((shoppingList [ 
			(A, [Common (Same B, Common (Except (Items [1;2;3;4;5], [1;3;5]), Same C)); Items [2;4;8]]); 
			(B, [Except (Except (Except (Same A, [1]),[1;2]),[3]); Items [3;6;9]]); 
			(C, [Same A; Same B; Same D; Same E]); 
			(D, [Items [10]; Common (Same A, Same D); Items [5]]); 
			(E, [Common (Same C, Common (Same A, Common (Same D, Same B)))]) 
			]) = [(A, [2; 4; 8]); (B, [3; 4; 6; 8; 9]); (C, [2; 3; 4; 5; 6; 8; 9; 10]); (D, [5; 10]); (E, [])]); 
print_endline "10"; 

assert ((shoppingList [ 
			(A, [Items [1;2;3;1;2;3]]); 
			(D, [Items [5;5;5;5;5]]); 
			(A, [Same D]); 
			(E, [Except (Items [1;2;3;1;2;3], [1;2;3])]); 
			(A, [Items [1;2;3;4]]); 
			]) = [(A, [1; 2; 3; 4; 5]); (B, []); (C, []); (D, [5]); (E, [])]); 

print_endline "pass all tests"; 
*)

