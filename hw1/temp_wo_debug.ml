
(* exercise 1 *)

let rec sigma : int * int * (int->int) -> int = fun (a, b, f) ->
	if a==b then (f a)
	else (f a) + (sigma (a+1, b, f))



(* exercise 2 *)

let sumprod : (int*int->float) * int * int -> float = fun (m, n, k) ->
	let rec loop_sum i = 
		if i == n+1 then 0.
		else let rec loop_mult j =
			if j == k+1 then 1.
			else (m(i, j)) *.  (loop_mult(j+1)) in
		(loop_mult 1) +. (loop_sum(i+1)) in
	loop_sum 1



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

