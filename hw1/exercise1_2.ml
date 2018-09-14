
(* 2015-18525 Sehoon Kim *)

(* exercise 2 *)

let sumprod : (int*int->float) * int * int -> float = fun (m, n, k) ->

	(* range exceptions *)
	if k < 1 then 1.0
	else if n < 1 then 0.0

	else let rec loop_sum i = 
		if i == n+1 then 0.
		else let rec loop_mult j =
			if j == k+1 then 1.
			else (m(i, j)) *. (loop_mult(j+1)) in
		(loop_mult 1) +. (loop_sum(i+1)) in
	loop_sum 1


(* debugging 2 

let _ = print_endline "DEBUG2"

let f1 : int*int -> float = fun (x, y) -> float_of_int (x+y)
let f2 : int*int -> float = fun (x, y) -> float_of_int (x+y+y)

let ss1 = sumprod(f1, 3, 5)
let ss2 = sumprod(f2, 3, 5)
let _ = print_endline (string_of_float ss1)
let _ = print_endline (string_of_float ss2)


let matrix (i, j) = ((float_of_int i) *. 10.) +. (float_of_int j)
let _ = print_endline (string_of_float (sumprod (matrix, 2, 7)))
*)
