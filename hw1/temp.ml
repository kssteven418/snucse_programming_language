
(* exercise 1 *)

let rec sigma : int * int * (int->int) -> int = fun (a, b, f) ->
	if a==b then (f a)
	else if a > b then 0
	else (f a) + (sigma (a+1, b, f))

		
(* debugging 1 *)

let double = fun x -> x * 2
let square = fun x -> x * x
let plus1 = fun x -> x+1

let a = 100
let b = 101

let s1 = sigma (a, b, double)
let s2 = sigma (a, b, square)
let s3 = sigma (a, b, plus1)

let _ = print_endline (string_of_int s1)
let _ = print_endline (string_of_int s2)
let _ = print_endline (string_of_int s3)


(* exercise 2 *)

let sumprod : (int*int->float) * int * int -> float = fun (m, n, k) ->
	
	if k < 1 then 1.0
	else if n < 1 then 0.0
	else let rec loop_sum i = 
		if i == n+1 then 0.
		else let rec loop_mult j =
			if j == k+1 then 1.
			else (m(i, j)) *.  (loop_mult(j+1)) in
		(loop_mult 1) +. (loop_sum(i+1)) in
	loop_sum 1


(* debugging 2 *)

let _ = print_endline "DEBUG2"

let f1 : int*int -> float = fun (x, y) -> float_of_int (x+y)
let f2 : int*int -> float = fun (x, y) -> float_of_int (x+y+y)

let ss1 = sumprod(f1, 3, 5)
let ss2 = sumprod(f2, 3, 5)
let _ = print_endline (string_of_float ss1)
let _ = print_endline (string_of_float ss2)


let matrix (i, j) = ((float_of_int i) *. 10.) +. (float_of_int j)
let _ = print_endline (string_of_float (sumprod (matrix, 2, (-1))))


(* excercise 3 *)

type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon | Poland | Portugal 
          | Italy | Germany | Norway | Sweden | England | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let rec parenize : tourna -> string = fun t -> 
	match t with
	|NODE (left, right) -> "(" ^ (parenize left) ^ " " ^ (parenize right) ^ ")"
	|LEAF leaf -> 
		(match leaf with
		 |Korea -> "Korea"
		 |France -> "France"
		 |Usa -> "Usa"
		 |Brazil -> "Brazil"
		 |Japan -> "Japan"
		 |Nigeria -> "Nigiria"
		 |Cameroon -> "Cameroon"
		 |Poland -> "Poland"
		 |Portugal -> "Portugal"
		 |Italy -> "Italy"
		 |Germany -> "Germany"
		 |Norway -> "Norway"
		 |Sweden -> "Swenden"
		 |England -> "England"
		 |Argentina -> "Argentina")


(* debugging 3 *)

let t_temp = NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)
let s_temp = parenize(t_temp)
let _ = print_endline s_temp


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
let _ = print_endline "EX_4"
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

let s = eval (LESS(NUM 1, MINUS(NUM 0, NUM 0)))
let _ = print_endline (string_of_bool s)
let s = eval (LESS(NUM 1, MINUS(NUM 2, NUM 0)))
let _ = print_endline (string_of_bool s)
let s = eval (LESS(NUM 1, MINUS(NUM 0, NUM 2)))
let _ = print_endline (string_of_bool s)
let s = eval (LESS(PLUS(NUM 0, NUM 2), MINUS(PLUS(NUM 1, NUM 2), NUM 1)))
let _ = print_endline (string_of_bool s)
	

(* exercise 5 *)

type nat = ZERO | SUCC of nat 

let rec natadd : nat * nat -> nat = fun (x, y) ->
	match x with
	|ZERO -> y
	|SUCC s -> SUCC(natadd (s,y))

let rec natmul : nat * nat -> nat = fun (x, y) ->
	match x with 
	|ZERO -> ZERO
	|SUCC s -> natadd (natmul (s, y), y)

(* debugging 5 *)

let _ = print_endline "EX_5"
let rec nat_to_int : nat -> int = fun x ->
	match x with
	|ZERO -> 0
	|SUCC x -> nat_to_int(x) + 1

let n4 = SUCC(SUCC(SUCC(SUCC(ZERO))))
let n2 = SUCC(SUCC(ZERO))
let n1 = SUCC(ZERO)
let n0 = ZERO

(*
let n0_0 = natadd (n0, n0)
let n0_1 = natadd (n0, n1)
let n0_2 = natadd (n0, n2)
let n1_2 = natadd (n1, n2)
let n2_4 = natadd (n2, n4)
let n4_4 = natadd (n4, n4)
*)

let n0_0 = natadd (n0, n0)
let n0_1 = natadd (n1, n0)
let n0_2 = natadd (n2, n0)
let n1_2 = natadd (n2, n1)
let n2_4 = natadd (n4, n2)
let n4_4 = natadd (n4, n4)

let n0_0 = natmul (n0, n0)
let n0_1 = natmul (n1, n0)
let n0_2 = natmul (n2, n0)
let n1_2 = natmul (n2, n1)
let n2_4 = natmul (n4, n2)
let n4_4 = natmul (n4, n4)

let n0_0 = natmul (n0, n0)
let n0_1 = natmul (n0, n1)
let n0_2 = natmul (n0, n2)
let n1_2 = natmul (n1, n2)
let n2_4 = natmul (n2, n4)
let n4_4 = natmul (n4, n4)

let i0_0 = nat_to_int(n0_0)
let i0_1 = nat_to_int(n0_1)
let i0_2 = nat_to_int(n0_2)
let i1_2 = nat_to_int(n1_2)
let i2_4 = nat_to_int(n2_4)
let i4_4 = nat_to_int(n4_4)

let _ = print_endline (string_of_int i0_0)
let _ = print_endline (string_of_int i0_1)
let _ = print_endline (string_of_int i0_2)
let _ = print_endline (string_of_int i1_2)
let _ = print_endline (string_of_int i2_4)
let _ = print_endline (string_of_int i4_4)

