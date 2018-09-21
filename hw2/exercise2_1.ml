

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


let calculate : exp -> float = fun e ->

	let rec cal : exp * int * float -> float = fun (e, mode, x) ->
		(* mode 0 : disable X, 1 : enable X *)
		match e with 
		| X -> if mode == 0 then raise FreeVariable
			   else x
		| INT n -> float_of_int n
		| REAL n -> n
		| ADD (e1, e2) -> (cal (e1, mode, x)) +. (cal (e2, mode, x))
		| SUB (e1, e2) -> (cal (e1, mode, x)) -. (cal (e2, mode, x))
		| MUL (e1, e2) -> (cal (e1, mode, x)) *. (cal (e2, mode, x))
		| DIV (e1, e2) -> (cal (e1, mode, x)) /. (cal (e2, mode, x))
		
		| SIGMA (st, en, form) -> 
			let st_int = (int_of_float (cal (st, mode, x))) 
			and en_int = (int_of_float (cal (en, mode, x))) in
				(* if start point is bigger than the end point *)
				if st_int > en_int then 0. 
				(* else *)
				else let rec iter i = 
					if i > en_int then 0.
					else (cal (form, 1, (float_of_int i))) +. (iter (i+1)) in
					iter st_int
		
		| INTEGRAL (st, en, form) ->
			let st_real = cal (st, mode, x) 
			and en_real = cal (en, mode, x) in
			
			(* |st-en| < 0.1 then return 0. *)
			if st_real -. en_real > 0. && st_real -. en_real < 0.1 then 0.
			else if en_real -. st_real > 0. && en_real -. st_real < 0.1 then 0.
			
			(* if st>en then return integral *)
			else let cal_integral : float * float * exp -> float 
				= fun (st, en, form) ->
				(* en is alwas larger then st+0.1 *)
				
				let rec iter i =
					(* if i is bigger than the upper bound en *)
					if i > en then 0.
					(* if i~en is smaller than dx, *)
					(* than the width should be (en-i), NOT dx *)
					else if (i +. dx) > en 
						then ((cal (form, 1, i)) *. (en -. i)) +. (iter (i +. dx))
					(* otherwise, the width is equal to dx *)
					else (cal (form, 1, i) *. dx) +. (iter (i +. dx)) in
					iter st in
			
			(*if st<en *)
			if en_real > st_real then cal_integral(st_real, en_real, form)
			(* if st>en then return -integral *)
			else (0. -. cal_integral(en_real, st_real, form)) 
		in
		cal (e, 0, 0.)



(* DEBUGGING*)

(*
let e1 = SIGMA(INT 1, INT 4, SUB(ADD(X, X), INT 1))
let e2 = SIGMA(INT 1, INT 8, DIV(MUL(X, X), REAL 2.5))
let e3 = ADD(MUL(X, X), REAL 1.5)
let _ = calculate e1
let _ = calculate e2
let test = calculate(SIGMA(e1, e2, e3))
*)

(*
let e1 = (ADD(INT 1, DIV(REAL 60., INT 3)))
let e2 = (SIGMA(INT 1, INT 4, MUL(X, X)))
let _ = calculate e1
let _ = calculate e2
let e3 = SIGMA(e1, e2, DIV(X, REAL 5.0))
let _ = calculate e3
*)

(*
let e1 = SIGMA(X, INT 4, MUL(X, X))
let e2 = SIGMA(INT 1, INT 3, e1)
let _ = calculate(e2)

let e1 = SIGMA(X, MUL(INT 2, X), MUL(X, X))
let e2 = SIGMA(INT 1, INT 2, e1)
let _ = calculate(e2)

let e1 = SIGMA (REAL 1.0, REAL 5.0, MUL(X,  SIGMA(REAL 1.0, REAL 2.0, X)))
let _ = calculate e
*)

(*
let _ = calculate(INTEGRAL(REAL 1.0, REAL (-5.0), X))

let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 1.09, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 0.91, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 1.1, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 0.8999, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 1.2, MUL(X, X))))
let _ = string_of_float(calculate(INTEGRAL(REAL 1.0, REAL 0.8, MUL(X, X))))
*)

(*
let e1 = ADD(INT 1, DIV(REAL 60., INT 3))
let e2 = SIGMA(INT 1, INT 4, MUL(X, X))
let _ = calculate e1
let _ = calculate e2
let _ = calculate(INTEGRAL(e1, e2, INT 2))
let _ = calculate(INTEGRAL(INT 0, INT 2, MUL(X, X)))
let _ = calculate(INTEGRAL(e1, e2, INTEGRAL(INT 0, INT 2, MUL(X, X))))
let _ = calculate(INTEGRAL(e1, e2, REAL 2.47))

let e = INTEGRAL(INT 1, INT 2, INTEGRAL(X, MUL(INT 2, X), ADD(INT 1, MUL(INT 3, MUL(X, X)))))
let _ = calculate e
*)
