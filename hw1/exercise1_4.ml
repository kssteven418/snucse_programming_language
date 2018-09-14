
(* 2015-18525 Sehoon Kim *)


(* exercise 4 *)

type expr = NUM of int 
	| PLUS of expr * expr
	| MINUS of expr * expr

type formula = TRUE
	| FALSE
	| NOT of formula
	| ANDALSO of formula * formula
	| ORELSE of formula * formula
	| IMPLY of formula * formula
	| LESS of expr * expr

let rec eval : formula -> bool = fun form ->
	match form with
	|TRUE -> true
	|FALSE -> false
	|NOT f -> not(eval f)
	|ANDALSO (x, y) -> (eval x) && (eval y)
	|ORELSE (x, y) -> (eval x) || (eval y)
	|IMPLY (x, y) -> not(eval x) || (eval y)
	|LESS (x, y) ->
		let rec eval_x x_temp = 
			match x_temp with
		 	|NUM n -> n
		 	|PLUS (n, m) -> (eval_x n) + (eval_x m)
		 	|MINUS (n, m) -> (eval_x n) - (eval_x m)
			in
		(eval_x x) < (eval_x y)


(* debugging 4 *)

(*
let s = eval (ANDALSO(FALSE, FALSE))
let _ = print_endline (string_of_bool s)
let s = eval (ANDALSO(FALSE, TRUE))
let _ = print_endline (string_of_bool s)
let s = eval (ANDALSO(TRUE, FALSE))
let _ = print_endline (string_of_bool s)
let s = eval (ANDALSO(TRUE, TRUE))
let _ = print_endline (string_of_bool s)
let s = eval (ORELSE(FALSE, FALSE))
let _ = print_endline (string_of_bool s)
let s = eval (ORELSE(FALSE, TRUE))
let _ = print_endline (string_of_bool s)
let s = eval (ORELSE(TRUE, FALSE))
let _ = print_endline (string_of_bool s)
let s = eval (ORELSE(TRUE, TRUE))
let _ = print_endline (string_of_bool s)
let s = eval (IMPLY(FALSE, FALSE))
let _ = print_endline (string_of_bool s)
let s = eval (IMPLY(FALSE, TRUE))
let _ = print_endline (string_of_bool s)
let s = eval (IMPLY(TRUE, FALSE))
let _ = print_endline (string_of_bool s)
let s = eval (IMPLY(TRUE, TRUE))
let _ = print_endline (string_of_bool s)
let s = eval (ORELSE(ANDALSO(TRUE, FALSE), ORELSE(FALSE, FALSE)))
let _ = print_endline (string_of_bool s)
*)

(*
let _ = print_endline ( string_of_bool (eval (LESS (PLUS(NUM 3, NUM 4), MINUS(NUM 7, NUM (-1))))))
let s = eval (LESS(NUM 1, MINUS(NUM 0, NUM 0)))
let _ = print_endline (string_of_bool s)
let s = eval (LESS(NUM 1, MINUS(NUM 2, NUM 0)))
let _ = print_endline (string_of_bool s)
let s = eval (LESS(NUM 1, MINUS(NUM 0, NUM 2)))
let _ = print_endline (string_of_bool s)
let s = eval (LESS(PLUS(NUM 0, NUM 2), MINUS(PLUS(NUM 1, NUM 2), NUM 1)))
let _ = print_endline (string_of_bool s)
*)	
