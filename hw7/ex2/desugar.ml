(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp

(* TODO : Implement this function *)

let cntf = ref 0 
let cntv = ref 0 

let unused_i = -1278643 
let unused = Num(unused_i) 


let fname = 
	let temp = "!@#$%^&*&^%$#@#$%^&*"^(string_of_int !cntf) in
	let _ = cntf := !cntf+1 in
	temp

let vname x = 
	let temp = "tt"^(string_of_int !cntv) in
	let _ = cntv := !cntv+1 in
	temp

(* LET x = e1 in e2 *)
let letx (x, e1, e2) = 
	App (Fn (x, e2), e1)

(* e 0 will be handler number, e 1 will be the true value *)
let build_ftn (e, n) =
	let v = vname 0 in
		Fn(v, If(Var(v), e, n))

(* is the sub expression was ended successful? *)
(* if successful, than the v[0] should be intact with unused value *)
let is_successful v =
	Equal(App(Var v, Equal(Num 1, Num 0)), unused)

(* the original value is kept in v[1] *)
let value v = App(Var v, Equal(Num 1, Num 1)) 

let rec removeIter : xexp -> xexp = fun e ->
	match e with 
	| Num n -> 
		let _ = print_endline "NUM" in
		build_ftn (Num n, unused)
	| Var v -> 
		let _ = print_endline "VAR" in
		build_ftn (Var v, unused)
	| Fn (x, e) -> 
		let _ = print_endline "FN" in
		removeIter e
	| App (e1, e2) ->
		let _ = print_endline "APP" in
		let e1' = removeIter e1 in
		let e2' = removeIter e2 in
		(* x <- e1' *)
		(* y <- e2' *)
		let x = vname 0 in
		let y = vname 0 in

		let final = App(value x, value y) in

		let body2 = If(is_successful y, build_ftn(final, unused), Var y) in
		let let2 = letx(y, e2', body2) in

		let body = If(is_successful x, let2, Var x) in
		letx(x, e1', body)

	| If (e1, e2, e3) ->
		let _ = print_endline "IF" in
		let e1' = removeIter e1 in
		let e2' = removeIter e2 in
		let e3' = removeIter e3 in
		(* x <- e1' *)
		(* y <- e2' *)
		(* z <- e3' *)
		let x = vname 0 in
		let y = vname 0 in
		let z = vname 0 in

		let final = If(value x, value y, value z) in
		
		let body3 = If(is_successful z, build_ftn(final, unused), Var z) in
		let let3 = letx(z, e3', body3) in

		let body2 = If(is_successful y, let3, Var y) in
		let let2 = letx(y, e2', body2) in

		let body = If(is_successful x, let2, Var x) in
		letx(x, e1', body)

	| Equal (e1, e2) ->
		let _ = print_endline "EQUAL" in
		let e1' = removeIter e1 in
		let e2' = removeIter e2 in
		(* x <- e1' *)
		(* y <- e2' *)
		let x = vname 0 in
		let y = vname 0 in

		let final = Equal(value x, value y) in

		let body2 = If(is_successful y, build_ftn(final, unused), Var y) in
		let let2 = letx(y, e2', body2) in

		let body = If(is_successful x, let2, Var x) in
		letx(x, e1', body)

	| Raise e -> 
		let _ = print_endline "RAISE" in
		let e' = removeIter e in
		(* x <- e' *)
		let x = vname 0 in
		let final = value x in
		
		let body = If(is_successful x, build_ftn(Num 0, final), Var x) in
		(match final with
		| Num n -> let _ = print_endline (string_of_int n) in 
		letx(x, e', body)
		| _ -> 
		letx(x, e', body)
		)
	| Handle (e1, x, e2) -> 
		removeIter e1

let removeExn : xexp -> xexp = fun e ->
	let temp = Fn(vname 0 , Num(201812)) in
	App (Fn (fname, (removeIter e)), temp) 


