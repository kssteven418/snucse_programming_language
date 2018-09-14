


(* exercise 1 *)

let dx : float = 0.1

type exp = X
		 | INT of int
		 | REAL of float
		 | ADD of exp * exp
		 | SUB of exp * exp
		 | MUL of exp * exp
		 | DIV of exp * exp
		 | SIGMA of exp * exp * exp
		 | INTEGRAL of exp * exp * exp

let rec calculate : exp -> float = fun e ->
	match e with
	|X -> 0. (* NOT A POSSIBLE INPUT *)
	|INT n -> float_of_int n
	|REAL n -> n
	|ADD (n, m) -> (calculate n) +. (calculate m)
	|SUB (n, m) -> (calculate n) -. (calculate m)
	|MUL (n, m) -> (calculate n) *. (calculate m)
	|DIV (n, m) -> (calculate n) /. (calculate m)
	|SIGMA (st, en, ftn) ->
		let rec sum i = 
			(* if iteration is over the upper bound *)
			if i > (match en with |INT n -> n |_ -> 0) then 0. 
			(* else id iteration is under the upper bound *)
			else let rec eval f =
				match f with
				|X -> float_of_int i (* x value at ith iteration *)
				|INT n -> float_of_int n
				|REAL n -> n
				|ADD (n, m) -> (eval n) +. (eval m)
				|SUB (n, m) -> (eval n) -. (eval m)
				|MUL (n, m) -> (eval n) *. (eval m)
				|DIV (n, m) -> (eval n) /. (eval m)
				|_ -> 0. (* NOT POSSIBLE INPUTS *) in
			(eval ftn) +. (sum (i+1)) in
		sum (match st with |INT n -> n |_ -> 0)
	|INTEGRAL (st, en, ftn) -> 
		let rec sum i =
			(* if iteration is over the upper bound *)
			if i > (match en with |REAL n -> n |_ -> 0.) then 0. 
			(* else id iteration is under the upper bound *)
			else let rec eval f =
				match f with
				|X -> i (* leftmost x value at dx interval *)
				|INT n -> float_of_int n
				|REAL n -> n
				|ADD (n, m) -> (eval n) +. (eval m)
				|SUB (n, m) -> (eval n) -. (eval m)
				|MUL (n, m) -> (eval n) *. (eval m)
				|DIV (n, m) -> (eval n) /. (eval m)
				|_ -> 0. (* NOT POSSIBLE INPUTS *) in
			((eval ftn) *. dx) +. (sum (i +. dx)) in
		sum (match st with |REAL n -> n |_ -> 0.)

(* debuggin 1 *)

let s = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))
let _ = print_endline (string_of_float (calculate s))
let s = INTEGRAL(REAL 1., REAL 10., SUB(MUL(X, X), INT 1))
let _ = print_endline (string_of_float (calculate s))



(* exercise 2 *)


let s = SIGMA(INT 1, INT 10, SUB(MUL(X, X), INT 1))
let _ = print_endline (string_of_float (mathemadiga s))


type ae = CONST of int
 		| VAR of string
		| POWER of string * int
		| TIMES of ae list
		| SUM of ae list

let diff : ae * string -> ae = fun (a, st) ->
	match ae with
	| VAR v ->  
	
