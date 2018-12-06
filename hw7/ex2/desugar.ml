(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp

(* TODO : Implement this function *)

let cntf = ref 0 
let cntv = ref 0 


let vname () = 
	let temp = "["^(string_of_int !cntv)^"]" in
	let _ = cntv := !cntv+1 in
	temp

let unused_i = 201812
let unused = Num unused_i

(*


(* is the sub expression was ended successful? *)
(* if successful, than the v[0] should be intact with unused value *)
let is_successful v =
	Equal(App(Var v, Equal(Num 1, Num 0)), unused)

let is_match (v, w)  =
	Equal(App(v, Equal(Num 1, Num 0)), App(w, Equal(Num 1, Num 0)))

let get_num v = 
	App(Var v, Equal(Num 1, Num 0))

(* the original value is kept in v[1] *)
let value v = App(Var v, Equal(Num 1, Num 1)) 

*)
let rec print e =
	match e with 
	| Num n -> string_of_int n
	| Var v -> v
	| Fn (x, e') -> x^"->"^(print e')
	| App (e1, e2) -> "(("^(print e1)^") ("^(print e2)^"))"
	| If (e1, e2, e3) -> "if ( "^(print e1)^" ) \n then( "^(print e2)^" ) \n else( "^(print e3)^" )"
	| Equal (e1, e2) -> "("^(print e1)^" = "^(print e2)^")"
	| _ -> "Non"

(* LET x = e1 in e2 *)
let letx (x, e1, e2) = 
	App (Fn (x, e2), e1)

(*
	let k = vname() in
	let k1 = vname() in
	let k2 = vname() in
	let v1 = vname() in
	let v2 = vname() in
	Fn (k, App(Fn(k1, App (Var k1, n)),
					Fn(v1, App( Fn(k2, App(Var k2, unused)),
									Fn(v2, App(Var k, Equal(Var v1, Var v2)))))))
*)
(* will be binded later *)
let one_v = vname()
let zero_v = vname()
let true_v = Equal(Num 1, Num 1)
let false_v = Equal(Num 1, Num 0)

let cpsnum n = 
		let k = vname() in
		Fn (k, App(Var k, n)) 

let cpsvar v = 
		let k = vname() in
		Fn (k, App(Var k, v)) 
	
let cpsfn (x, e') =
		let k = vname() in
		Fn(k, App(Var k, Fn(x, e'))) 

let cpsapp (e1', e2') = 
		let k = vname() in
		let v1 = vname() in
		let v2 = vname() in
		Fn (k,  App (e1',
								Fn (v1, App (e2',
												Fn (v2, App (App (Var v1, Var v2), Var k)))))) 

let cpsif (e1', e2', e3') = 
    let k = vname () in
    let v1 = vname () in
    let v2 = vname () in
		let v3 = vname () in
		let e2'' = App (e2', Fn (v2, App (Var k, Var v2))) in
		let e3'' = App (e3', Fn (v3, App (Var k, Var v3))) in
		Fn (k, App (e1', Fn(v1, If (Var v1, e2'', e3'')))) 

let cpseq (e1', e2') = 
    let k = vname () in
    let v1 = vname () in
    let v2 = vname () in
    Fn (k, App (e1', 
								Fn (v1, App (e2', 
												Fn (v2, App (Var k,  Equal(Var v1, Var v2))))))) 

let one_f = cpsnum (Num 1)
let zero_f = cpsnum (Num 0) 

let true_f = cpseq(one_f, one_f)
let false_f = cpseq(one_f, zero_f)

let is_succ n' =
	cpseq(n', cpsnum(unused))

let rec cps : xexp -> xexp = fun e ->

	let k = vname() in

(* e 0 will be handler number, e 1 will be the true value *)
	let build_ftn (e', n) =
		let n' = cpsnum n in
		let v = vname() in
		let ifst' = cpsif(cpsvar(Var v), e', n') in
		let f' = cpsfn(v, ifst') in
		f' in

	let build_ftn' (e', n') =
		let v = vname() in
		let ifst' = cpsif(cpsvar(Var v), e', n') in
		let f' = cpsfn(v, ifst') in
		f' in
	(*
		let v = vname () in
		let k = vname () in
		let k1 = vname () in
		let k2 = vname () in
		let n' = Fn (k2, App (Var k, n)) in
		let body =
			Fn(k, App(Var k, Fn(v,
						Fn(k1, App(e', Fn(k1, If(Var k1, e', n'))))))) in
		body in
	*)
	(*
		let v = vname() in
		Fn(v, If(Var v, e', n)) in
	*)
		
	match e with 
	| Num n -> 
		let n' = Fn (k, App (Var k, Num n)) in
		build_ftn(n', unused)
	
	| Var v ->
		let v' = Fn (k, App (Var k, Var v)) in
		build_ftn(v', unused)
	
	| Fn (x, e) ->
		let _ = print_endline "FN" in
		let e' = cps e in
		let f = cpsapp(e', true_f) in
		let n = cpsapp(e', false_f) in

		let f' = cpsfn(x, f) in
		let f_succ = build_ftn(f', unused) in
		let f_fail = e' in
		let f'' = cpsif(is_succ n, f_succ, f_fail) in 
		f''
		(*build_ftn(f', unused)*)

	
	| App (e1, e2) ->
		let _ = print_endline "APP" in

		let e1' = cps e1 in
		let e2' = cps e2 in
		let f1 = cpsapp(e1', true_f) in
		let n1 = cpsapp(e1', false_f) in
		let f2 = cpsapp(e2', true_f) in
		let n2 = cpsapp(e2', false_f) in

		let f' = cpsapp(f1, f2) in 
		let f_succ = build_ftn(f', unused) in
		
		let f'' = cpsif(is_succ n1, cpsif(is_succ n2, f_succ, e2'), e1') in
		f''

	| Raise e ->
		let e' = cps e in
		let f = cpsapp(e', true_f) in
		let n = cpsapp(e', false_f) in
		(* TODO :  handlinf nested raise..?? *)

		build_ftn'(n, f)

	|_ -> e

let removeExn : xexp -> xexp = fun e ->
	let k = vname() in
	let temp = 
		(* If(App((is_succ (Num 10)),(Fn(k, Var k))), Num 1, Num 0) in*)
		App(cpsapp(cps e, true_f), (Fn(k, Var k))) in

	temp
		(*
	| Num n -> 
		let _ = print_endline "NUM" in
		build_ftn (Num n, unused)

	| Var v -> 
		let _ = print_endline "VAR" in
		build_ftn (Var v, unused)

	| Fn (x, e) -> 
		let e' = removeIter e in
		let y = vname 0 in
		let _ = print_endline ("FN "^y) in

		let final = Fn(x, value y) in

		let body = If(is_successful y, build_ftn(final, unused), Var y) in
		letx(y, e', body)

	| App (e1, e2) ->
		let e1' = removeIter e1 in
		let e2' = removeIter e2 in
		(* x <- e1' *)
		(* y <- e2' *)
		let x = vname 0 in
		let y = vname 0 in
		let _ = print_endline ("APP "^x^" "^y) in
		
		let _ = print_endline (print e1') in
		let _ = print_endline "" in
		let _ = print_endline (print e2') in
		let _ = print_endline "" in

		let final = App(value x, value y) in

		let body2 = If(is_successful y, build_ftn(final, unused), Var y) in
		let let2 = letx(y, e2', body2) in

		let body = If(is_successful x, let2, Var x) in
		letx(x, e1', body)

	| If (e1, e2, e3) ->
		let e1' = removeIter e1 in
		let e2' = removeIter e2 in
		let e3' = removeIter e3 in
		(* x <- e1' *)
		(* y <- e2' *)
		(* z <- e3' *)
		let x = vname 0 in
		let y = vname 0 in
		let z = vname 0 in
		let _ = print_endline ("IF "^x^" "^y^" "^z) in

		let final = If(value x, value y, value z) in
		
		let body3 = If(is_successful z, build_ftn(final, unused), Var z) in
		let let3 = letx(z, e3', body3) in

		let body2 = If(is_successful y, let3, Var y) in
		let let2 = letx(y, e2', body2) in

		let body = If(is_successful x, let2, Var x) in
		letx(x, e1', body)

	| Equal (e1, e2) ->
		let e1' = removeIter e1 in
		let e2' = removeIter e2 in
		(* x <- e1' *)
		(* y <- e2' *)
		let x = vname 0 in
		let y = vname 0 in
		let _ = print_endline ("EQUAL "^x^" "^y) in

		let final = Equal(value x, value y) in

		let body2 = If(is_successful y, build_ftn(final, unused), Var y) in
		let let2 = letx(y, e2', body2) in

		let body = If(is_successful x, let2, Var x) in
		letx(x, e1', body)

	| Raise e -> 
		let e' = removeIter e in
		(* x <- e' *)
		let x = vname 0 in
		let _ = print_endline ("RAISE "^x) in

		let final = value x in
		
		let body = If(is_successful x, build_ftn(unused, final), Var x) in
		letx(x, e', body)

	| Handle (e1, x, e2) -> 
		let _ = print_endline "HANDLE" in
		let e1' = removeIter e1 in
		let e2' = removeIter e2 in
		(* y <- e1' *)
		let y = vname 0 in 

		let body = If(Equal(get_num y, Num x), build_ftn(e2', get_num y), Var y) in
		let _ = print_endline ("HANDLE2"^y) in
		letx(y, e1', body)
		*)


						




