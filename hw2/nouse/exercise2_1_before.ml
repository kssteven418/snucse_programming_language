


(* exercise 2-1 *)
(* calculator *)

exception FreeVariable

let dx = 0.1

type exp = X
	| INT of int
	| REAL of float
	| ADD of exp * exp
	| SUB of exp * exp
	| MUL of exp * exp
	| DIV of exp * exp
	| SIGMA of exp * exp * exp
	| INTEGRAL of exp * exp * exp

(* evaluate formula(function) of x with the given value x *)
let rec eval_formula : exp * float -> float = fun (form, x) ->
	match form with
	| X -> x
	| INT n -> float_of_int n
	| REAL n -> n
	| ADD (e1, e2) -> (eval_formula (e1, x)) +. (eval_formula (e2, x))
	| SUB (e1, e2) -> (eval_formula (e1, x)) -. (eval_formula (e2, x))
	| MUL (e1, e2) -> (eval_formula (e1, x)) *. (eval_formula (e2, x))
	| DIV (e1, e2) -> (eval_formula (e1, x)) /. (eval_formula (e2, x))
	| _ -> 0. (* SUM and INTEGRAL in the formula is ignored tentatively*)

(* caluculate formula (function) and sum from st to en *)
let cal_sigma : int * int * exp -> float = fun (st, en, form) ->
	(* if start point is bigger than the end point *)
	if st > en then 0.
	(* else *)
	else let rec iter i =
		if i > en then 0.
		else (eval_formula (form, (float_of_int i))) +. (iter (i+1)) in
		iter st
	
(* caluculate formula (function) and integral from st to en *)
let cal_integral : float * float * exp -> float = fun (st, en, form) ->
	(* en is alwas larger then st+0.1 *)
	let rec iter i =
		(* if i is bigger than the upper bound en *)
		if i > en then 0.
		(* if i~en is smaller than dx, than the width should be (en-i), NOT dx *)
		else if (i +. dx) > en 
			then ((eval_formula (form, i)) *. (en -. i)) +. (iter (i +. dx))
		(* otherwise, the width is equal to dx *)
		else (eval_formula (form, i) *. dx) +. (iter (i +. dx)) in
		iter st
	

let rec calculate : exp -> float = fun e ->
	match e with
	| X -> raise FreeVariable
	| INT n -> float_of_int n
	| REAL n -> n
	| ADD (e1, e2) -> (calculate e1) +. (calculate e2)
	| SUB (e1, e2) -> (calculate e1) -. (calculate e2)
	| MUL (e1, e2) -> (calculate e1) *. (calculate e2)
	| DIV (e1, e2) -> (calculate e1) /. (calculate e2)
	| SIGMA (st, en, form) -> 
		let st_int = (int_of_float (calculate st)) 
		and en_int = (int_of_float (calculate en)) in
		cal_sigma (st_int, en_int, form)
	| INTEGRAL (st, en, form) ->
		let st_real = calculate st 
		and en_real = calculate en in
		(* |st-en| < 0.1 then return 0. *)
		if st_real -. en_real > 0. && st_real -. en_real < 0.1 then 0.
		else if en_real -. st_real > 0. && en_real -. st_real < 0.1 then 0.
		(* if st>en then return integral *)
		else if en_real > st_real then cal_integral(st_real, en_real, form)
		(* if st<en then return -integral *)
		else (0. -. cal_integral(en_real, st_real, form)) 



(* DEBUGGING*)

let e1 = SIGMA(INT 1, INT 4, SUB(ADD(X, X), INT 1))
let e2 = SIGMA(INT 1, INT 8, DIV(MUL(X, X), REAL 2.5))
let e3 = ADD(MUL(X, X), REAL 1.5)
let test = calculate(SIGMA(e1, e2, e3))

let _ = calculate(SIGMA(INT 1, INT 4, MUL(X, X)))
let _ = calculate(ADD(INT 10, DIV(REAL 60., INT 3)))
let _ = calculate(SIGMA(ADD(INT 10, DIV(INT 60, INT 3)), SIGMA(INT 1, INT 4, MUL(X, X)), DIV(X, REAL 5.0)))
(*
*)

(*
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 1.09, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 0.91, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 1.1, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 0.8999, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 1.2, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 0.8, MUL(X, X))))
*)

let e1 = ADD(INT 1, DIV(REAL 60., INT 3))
let e2 = SIGMA(INT 1, INT 4, MUL(X, X))
let _ = calculate e1
let _ = calculate e2
let _ = calculate(INTEGRAL(e1, e2, INT 2))
let _ = calculate(INTEGRAL(INT 0, INT 2, MUL(X, X)))
let _ = calculate(INTEGRAL(e1, e2, INTEGRAL(INT 0, INT 2, MUL(X, X))))
let _ = calculate(INTEGRAL(e1, e2, REAL 2.45))
