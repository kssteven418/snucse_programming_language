

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
	| NODE(l_r, l_v, l_lh, l_rh) ->
		(match rh with
		 | EMPTY -> lh (*if right heap is EMPTY *)
		 | NODE(r_r, r_v, r_lh, r_rh) ->
			if l_v < r_v then
				shake (l_v, l_lh, merge (l_rh, rh))
			else
				shake (r_v, r_lh, merge (lh, r_rh))
		)

let insert(x,h) = merge(h, NODE(0,x,EMPTY,EMPTY)) 

let deleteMin h = match h with 
                | EMPTY -> raise EmptyHeap 
	            | NODE(_,x,lh,rh) -> merge (lh,rh) 


(* debugging for the TEST CASE *)	

(*
let heap1 = NODE (1, 1, NODE (0, 5, EMPTY, EMPTY), NODE (0, 3, EMPTY, EMPTY)) 
let heap2 = NODE (1, 2, NODE (0, 4, EMPTY, EMPTY), NODE (0, 6, EMPTY, EMPTY)) 
let heap2 = NODE (0, 2, NODE (0, 4, EMPTY, EMPTY), EMPTY) 
*)

(*	
let heap1 = NODE (1, 1, NODE (1, 6, NODE (0, 10, EMPTY, EMPTY), NODE(0, 8, EMPTY, EMPTY)), NODE (0, 4, NODE(0, 5, EMPTY, EMPTY), EMPTY)) 
let heap2 = NODE (1, 2, NODE (0, 8, NODE (0, 11, EMPTY, EMPTY), EMPTY), NODE (0, 3, NODE(0, 7, EMPTY, EMPTY), EMPTY)) 
let tree = merge (heap1, heap2)
let _ = findMin(merge (heap1, heap2))

let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
let tree = deleteMin tree
let min = findMin(tree)
*)

(*
let tree = EMPTY

let tree = insert(1, tree)
let tree = insert(2, tree)
let tree = insert(4, tree)
let tree = insert(3, tree)
let tree = insert(0, tree)
let tree = insert(6, tree)
let tree = insert(10, tree)
let tree = insert(8, tree)
let tree = insert(7, tree)
let tree = insert(9, tree)
let tree = insert(11, tree)
let tree = insert(12, tree)
let tree = insert(13, tree)
let tree = insert(14, tree)
*)
