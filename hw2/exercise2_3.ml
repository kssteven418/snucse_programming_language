

type rank = int
type value = int
type heap = EMPTY | NODE of rank * value * heap * heap


exception EmptyHeap 

let rank h = match h with 
		   | EMPTY -> -1 
 		   | NODE(r,_,_,_) -> r 

let shake (x,lh,rh) = 
	  if (rank lh) >= (rank rh) 
	  then NODE(rank rh+1, x, lh, rh) 
	  else NODE(rank lh+1, x, rh, lh) 

let findMin h = match h with 
        	  | EMPTY -> raise EmptyHeap 
	          | NODE(_,x,_,_) -> x 


let rec merge : heap * heap -> heap = fun(lh, rh) ->
	match lh with 
	| EMPTY -> rh (* if left heap is EMPTY *)
	| NODE(l_r, l_v, l_rh, l_lh) ->
		(match rh with
		 | EMPTY -> lh (*if right heap is EMPTY *)
		 | NODE(r_r, r_v, r_rh, r_lh) ->
			if l_v < r_v then
				shake (l_v, l_lh, merge (l_rh, rh))
			else shake (r_v, r_lh, merge (r_rh, lh))
		)

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
                | EMPTY -> raise EmptyHeap 
	            | NODE(_,x,lh,rh) -> merge (lh,rh) 


(* debugging for the TEST CASE *)	

let heap1 = NODE (1, 1, NODE (0, 5, EMPTY, EMPTY), NODE (0, 3, EMPTY, EMPTY)) 
let heap2 = NODE (0, 2, NODE (0, 4, EMPTY, EMPTY), EMPTY) 
let _ = print_endline(string_of_int(findMin(merge (heap1, heap2))))

(* debugging *)
let tree = EMPTY

let tree = insert(1, tree)
let tree = insert(100, tree)
let tree = insert(10, tree)
let tree = insert(100000, tree)
let tree = insert(1000, tree)
let tree = insert(10000, tree)
let tree = insert(1000000000, tree)
let tree = insert(1000000, tree)
let tree = insert(100000, tree)
let tree = insert(10000000, tree)
let tree = insert(1000000000, tree)
let tree = insert(100000000, tree)

let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
let _ = print_endline (string_of_int (findMin tree))
let tree = deleteMin tree
