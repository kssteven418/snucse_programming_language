


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
	type queue = (int list list * int list list)
	
	exception EMPTY_Q
	
	let emptyQ = ([], [])
	
	let enQ = fun (q, e) ->
		let (pri, sec) = q in
			([e] @ pri, sec) 
	
	let deQ = fun q -> 
		let (pri, sec) = q in
			(* if the secondary stack is not full *)
			if (List.length sec) != 0 then
				((List.hd sec), (pri, List.tl sec))
			
			(* if both the primary and the secondary queues are empty *)
			(* then, raise the exception *)
			else if (List.length pri) == 0 then
				raise EMPTY_Q

			(* if only the secondary queue is empty *)
			(* then, first move all the elmt from the pri to the sec *)
			else let rec move p = 
				if (List.length p) == 0 then []
				else move(List.tl p) @ [List.hd p] in
				(List.hd (move pri), ([], List.tl (move pri)))
end

let printList lis = 
	let rec printRec l =
		if (List.length l) == 0 then ""
		else if (List.length l) == 1 then (string_of_int (List.hd l))
		else (string_of_int (List.hd l)) ^ ";" ^ (printRec (List.tl l)) in
	"[" ^ (printRec lis) ^ "]"
	

let toString q = 
	let (p, s) = q in
		let rec printQ queue =
			if (List.length queue) == 0 then ""
			else if (List.length queue) == 1 
				then (printList (List.hd queue)) 
			else "" ^ (printList (List.hd queue)) ^ ";" ^ (printQ (List.tl queue)) ^ ""  in
		"([" ^ (printQ p) ^ "], [" ^ (printQ s) ^ "])"


let q = IntListQ.emptyQ
let _ = print_endline (toString q)

let q = IntListQ.enQ(IntListQ.emptyQ, [1;2;3])
let _ = print_endline (toString q)


let q = IntListQ.enQ(q, [4;5])
let _ = print_endline (toString q)


let (n, q) = IntListQ.deQ(q)
let _ = print_endline (printList n)
let _ = print_endline (toString q)

let q = IntListQ.enQ(q, [6; 7])
let _ = print_endline (toString q)

let q = IntListQ.enQ(q, [8; 9])
let _ = print_endline (toString q)

let q = IntListQ.enQ(q, [10])
let _ = print_endline (toString q)

let (n, q) = IntListQ.deQ(q)
let _ = print_endline (printList n)
let _ = print_endline (toString q)

let (n, q) = IntListQ.deQ(q)
let _ = print_endline (printList n)
let _ = print_endline (toString q)

let q = IntListQ.enQ(q, [11;12;13])
let _ = print_endline (toString q)

let q = IntListQ.enQ(q, [14])
let _ = print_endline (toString q)

let (n, q) = IntListQ.deQ(q)
let _ = print_endline (printList n)
let _ = print_endline (toString q)

let (n, q) = IntListQ.deQ(q)
let _ = print_endline (printList n)
let _ = print_endline (toString q)

let (n, q) = IntListQ.deQ(q)
let _ = print_endline (printList n)
let _ = print_endline (toString q)

let (n, q) = IntListQ.deQ(q)
let _ = print_endline (printList n)
let _ = print_endline (toString q)

let (n, q) = IntListQ.deQ(q)
let _ = print_endline (printList n)
let _ = print_endline (toString q)

