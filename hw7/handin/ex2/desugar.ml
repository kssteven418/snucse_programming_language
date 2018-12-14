(*
 * SNU 4190.310 Programming Languages 
 * Homework "Exceptions are sugar" Skeleton
 *)

open Xexp

(* TODO : Implement this function *)

let cntv = ref 1

let new_var () = 
	let temp = "["^(string_of_int !cntv)^"]" in
	let _ = cntv := !cntv+1 in
	temp

let rec alpha_conv exp subst = 
  match exp with
  | Num n -> Num n
  | Var x -> (try Var (List.assoc x subst) with Not_found -> Var x)
  | Fn (x, e) ->
    let x' = new_var () in
    let subst' = (x, x') :: subst in
    Fn (x', alpha_conv e subst')
  | App (e1, e2) -> App (alpha_conv e1 subst, alpha_conv e2 subst)
  | If (e1, e2, e3) -> 
    If (alpha_conv e1 subst, alpha_conv e2 subst, alpha_conv e3 subst)
	| Equal (e1, e2) -> Equal(alpha_conv e1 subst, alpha_conv e2 subst)
	| Raise e -> Raise(alpha_conv e subst)
	| Handle (e1, x, e2) -> Handle(alpha_conv e1 subst, x, alpha_conv e2 subst)


let un_i = 201812
let un = Num un_i (* unusing number *)

let decapsule =
	let v1 = new_var() in
	let v2 = new_var() in
	Fn(v1, Fn(v2, Var v2))

let decapsule_deb = 
	let v1 = new_var() in
	let v2 = new_var() in
	Fn(v1, Fn(v2, Var v1))


let rec cps exp =
	
	let k = new_var () in
	
	(* encode :
	 	Normal expr : \k ((k un) val)  (un : unused #, 201812)
		Abnormal expr : \k ((k rn) hn) (rn : raise #, hn : handle #, init un) 
	*)

	match exp with
  | Num n -> Fn(k, App(App(Var k, un), Num n))
  | Var v -> Fn(k, App(App(Var k, un), Var v))
  | Fn (x, e) -> Fn(k, App(App(Var k, un), Fn(x, cps e))) 
  | App (e1, e2) -> 
		let rn1 = new_var() in
		let rn2 = new_var() in
		let f = new_var() in
		let v = new_var() in
		let subst_ftn f v = 
			let rn = new_var() in
			let hn = new_var() in
			let f' = App(Var f, Var v) in
			App(f', Fn(rn, If(Equal(Var rn, un),
												Fn(hn, App(App(Var k, Var rn), Var hn)),
												Fn(hn, App(App(Var k, Var rn), Var hn))))) in
		Fn(k, App((cps e1),
							Fn(rn1, If(Equal(Var rn1, un),
													Fn(f, App((cps e2),
																		Fn(rn2, If(Equal(Var rn2, un),
																								Fn(v, subst_ftn f v),
																								Fn(v, App(App(Var k, Var rn2), Var v)))))),
													Fn(f, App(App(Var k, Var rn1), Var f))))))

  | If (e1, e2, e3) -> 
		let rn1 = new_var() in
		let rn2 = new_var() in
		let rn3 = new_var() in
		let v1 = new_var() in
		let v2 = new_var() in
		let v3 = new_var() in

		(*
		Fn(k, App(cps e2, Fn(rn1, Fn(v1, App(App(Var k, Var v1
		*)
		Fn(k, App((cps e1),
							Fn(rn1, If(Equal(Var rn1, un),
													Fn(v1, If(Var v1,
																		App((cps e2), 
																				Fn(rn2, If(Equal(Var rn2, un),
																									Fn(v2, App(App(Var k, un), Var v2)),
																									Fn(v2, App(App(Var k, Var rn2), Var v2))))),
																		App((cps e3), 
																				Fn(rn3, If(Equal(Var rn3, un),
																									Fn(v3, App(App(Var k, un), Var v3)),
																									Fn(v3, App(App(Var k, Var rn3), Var v3))))))),
													Fn(v1, App(App(Var k, Var rn1), Var v1))))))
  | Equal (e1, e2) -> 
		let rn1 = new_var() in
		let rn2 = new_var() in
		let v1 = new_var() in
		let v2 = new_var() in
		Fn(k, App((cps e1),
							Fn(rn1, If(Equal(Var rn1, un),
													Fn(v1, App((cps e2),
																		Fn(rn2, If(Equal(Var rn2, un),
																								Fn(v2, App(App(Var k, un), Equal(Var v1, Var v2))),
																								Fn(v2, App(App(Var k, Var rn2), Var v2)))))),
													Fn(v1, App(App(Var k, Var rn1), Var v1))))))
  | Raise e -> 
		let rn = new_var () in 
		let hn = new_var () in
		(* case 1 : e is a normal expr, 
		 		then, make a new abnormal expression,
				by setting rn as a e's 2nd value and hn as un 
			 case 2 : e is a abnormal expr,
			 	then, pass it directly *)
		Fn(k, App((cps e), 
							Fn(rn, If(Equal(Var rn, un),
											  Fn(hn, App(App(Var k, Var hn), un)), (* 1 *)
												Fn(hn, App(App(Var k, Var rn), Var hn)))))) (* 2 *)
  | Handle (e1, x, e2) -> 
		let rn = new_var() in
		let v = new_var () in
		let v1 = new_var () in
		let v2 = new_var() in
		let handleftn =	Fn(v1, If(Equal(Var v1, un),
															Fn(v2, App(App(Var k, Var rn), Var v2)),
															Fn(v2, App(App(Var k, Var v1), un)))) in 
		Fn(k, App((cps e1),
							Fn(rn, If(Equal(Var rn, Num x),
												Fn(v, App(cps e2, handleftn)),
												Fn(v, App(App(Var k, Var rn), Var v)))))) 

let removeExn : xexp -> xexp = fun e ->
	let temp = cps (alpha_conv e [] ) in
	(*let first = App(temp, decapsule_deb) in*)
	let second = App(temp, decapsule) in
	second

