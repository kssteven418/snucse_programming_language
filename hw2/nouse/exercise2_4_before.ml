

module type Queue =
sig
	type element
	type queue
	exception EMPTY_Q
	val emptyQ: queue
	val enQ: queue * element -> queue
	val deQ: queue -> element * queue
end

module IntListQ =
struct
	type element = int list
	type queue = (int list * int list)
	exception EMPTY_Q
	
	let emptyQ = ([], [])
	
	let enQ = fun (q, e) ->
		let (lq, rq) = q in
			(* reverse the order of the input element list e and append to the left queue *)
			let rec rev inlist =
				if List.length inlist == 0 then []
				else (rev (List.tl inlist)) @ [List.hd inlist] in
			((rev e)@lq, rq) 
	
	let deQ = fun q ->
		let (lq, rq)= q in

			(*if right queue is not empty, dequeue from the low index!*)
			if (List.length rq) != 0 then
				([List.hd rq], (lq, List.tl rq))
			
			(*if left, right queue are both empty, exception!*)
			else if (List.length lq) == 0 then raise EMPTY_Q
			
			(* if only right queue is empty, move the elmts from the left queue to the right *)
			else let rec move left =
				if (List.length left == 0) then []
				else  move(List.tl left) @ [List.hd left] in
				([List.hd (move lq)], ([], List.tl (move lq)))
				
				
				
				
end

(*debugging*)	
module ValidIntListQ = (IntListQ : Queue)

let toString q = 
	let (left, right)=q in
		let rec print q =
			if (List.length q) == 0 then ""
			else (string_of_int (List.hd q)) ^ " " ^ (print (List.tl q)) in
		(print left) ^ "| " ^ (print right)  

let li = IntListQ.emptyQ
let _ = print_endline (toString li)
let li = IntListQ.enQ(li, [1])
let _ = print_endline (toString li)
let li = IntListQ.enQ(li, [2; 3; 4])
let _ = print_endline (toString li)
let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)
let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)


let li = IntListQ.enQ(li, [5; 6; 7])
let _ = print_endline (toString li)
let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)
let li = IntListQ.enQ(li, [8; 9])
let _ = print_endline (toString li)

let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)
let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)
let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)
let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)

let li = IntListQ.enQ(li, [10])
let _ = print_endline (toString li)
let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)
let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)


let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)
let (x, li) = IntListQ.deQ(li)
let _ = print_endline (string_of_int (List.hd x))
let _ = print_endline (toString li)

	

(*

let rec printList : int list  -> string = fun (l) ->
	if List.length l == 0 then " "
	else (string_of_int (List.hd l)) ^ " " ^ (printList (List.tl l))
module ValidIntListQ = (IntListQ : Queue)

let li = IntListQ.emptyQ
let _ = print_endline (printList li)
let li = IntListQ.enQ(li, [1])
let _ = print_endline (printList li)
let li = IntListQ.enQ(li, [2; 3; 4])
let _ = print_endline (printList li)
let li = IntListQ.emptyQ
let _ = print_endline (printList li)


let _ = print_endline (printList [1; 2; 3; 4])
*)

