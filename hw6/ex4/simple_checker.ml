(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton Code
 *)
(*
module M : sig
  type exp = CONST of const
           | VAR of id
           | FN of id * exp
           | APP of exp * exp
           | LET of decl * exp
           | IF of exp * exp * exp
           | BOP of bop * exp * exp
           | READ
           | WRITE of exp
           | MALLOC of exp          (*   malloc e *)
           | ASSIGN of exp * exp    (*   e := e   *)
           | BANG of exp            (*   !e       *)
           | SEQ of exp * exp       (*   e ; e    *)
           | PAIR of exp * exp      (*   (e, e)   *)
           | FST of exp            (*   e.1      *)
           | SND of exp            (*   e.2      *)
  and const = S of string | N of int | B of bool
  and id = string
  and decl = 
    | REC of id * id * exp  (* Recursive function decl. (fun_id, arg_id, body) *)
    | VAL of id * exp       (* Value decl, including non-recursive functions *)
  and bop = ADD | SUB | EQ | AND | OR

  (* types in M  *)
  type types = TyInt                     (* integer type *)
             | TyBool                    (* boolean type *)
             | TyString                  (* string type *)
             | TyPair of types * types   (* pair type *)
             | TyLoc of types            (* location type *)
             | TyArrow of types * types  (* function type *)
*)
open M
open Pp

exception UnbindError

type var = string

let count = ref 0 
let var_list = ref [] (* deb *)

let new_var () = 
  let _ = count := !count +1 in
	let _ = var_list := (!var_list)@["x_" ^ (string_of_int !count)] in (* deb *)
  "x_" ^ (string_of_int !count)

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
  (* Modify, or add more if needed *)

type tcon = 
	| IBS
	| IBSL

let rec str_of_type t = 
	match t with 
  | TInt -> "Int"
  | TBool -> "Bool"
  | TString -> "String"
  | TPair (t1, t2) -> 
		"("^(str_of_type t1)^", "^(str_of_type t2)^")"
  | TLoc t -> "Loc "^(str_of_type t)
  | TFun (t1, t2) -> 
		"("^(str_of_type t1)^"->"^(str_of_type t2)^")"
  | TVar v -> v

let print_type t =
	let _ = print_endline (str_of_type t) in
	()

let rec str_of_equ equ =
	if (List.length equ)=0 then ""
	else
		let (t1, t2) = List.hd equ in
		let str = "["^(str_of_type t1)^", "^(str_of_type t2)^"]" in
		str^" "^(str_of_equ (List.tl equ))

let print_equ equ = 
	let _ = print_endline (str_of_equ equ) in 
	()

(* id to variable *)
type glist = M.id -> typ
type equ = typ * typ

(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
	(* bind x|->tau at gamma *)
	let bind (gamma, x, tau) = fun t -> if t=x then tau else (gamma t) in


	(* BUILD EQUATIONS *)

	(* stores type constraints *)
	let type_constraint = ref [] in

	let rec v : glist * M.exp * typ -> equ list = fun (gamma, e, tau) ->
		match e with
		
		| M.CONST c -> 
			(match c with 
				| M.S s -> [(tau, TString)]
				| M.N n -> [(tau, TInt)] 
				| M.B b -> [(tau, TBool)] 
			)
		
		| M.VAR x -> [(tau, (gamma x))]
		
		| M.FN (x, exp) -> 
			let tau1 = new_var() in
			let tau2 = new_var() in
			let new_gamma = bind (gamma, x, TVar(tau1)) in
			[(tau, TFun(TVar(tau1), TVar(tau2)))] @ (* t = t1->t2 *)
			(v(new_gamma, exp, TVar(tau2))) (* x : a1 then exp must be a2 *)

		| M.APP (e1, e2) ->
			let tau1 = new_var() in
			v(gamma, e1, TFun(TVar(tau1), tau)) @
			v(gamma, e2, TVar(tau1))
		
		| M.LET (d, e2) ->
			(match d with
			| M.REC (f, x, e1) -> 
				let tau1 = new_var() in
				let tau2 = new_var() in
				let gamma_f = bind(gamma, f, TFun(TVar(tau1), TVar(tau2))) in
				let gamma_fx = bind(gamma_f, x, TVar(tau1)) in
				v(gamma_fx, e1, TVar(tau2)) 
				@v(gamma_f, e2, tau)

			| M.VAL (x, e1) -> 
				let tau1 = new_var() in
				let new_gamma = bind (gamma, x, TVar(tau1)) in
				v(gamma, e1, TVar(tau1)) @
				v(new_gamma, e2, tau)
			)

		| M.IF (e1, e2, e3) ->
			v(gamma, e1, TBool) @
			v(gamma, e2, tau) @
			v(gamma, e3, tau)

		| M.BOP (op, e1, e2) -> 
			(match op with 
				| M.ADD -> 
					[(tau, TInt)] @ v(gamma, e1, TInt) @ v(gamma, e2, TInt)
				| M.SUB ->
					[(tau, TInt)] @ v(gamma, e1, TInt) @ v(gamma, e2, TInt)
				| M.EQ -> 
					let tau' = new_var() in
					let _ = type_constraint := 
						(!type_constraint)@[(TVar(tau'), IBSL)] in
					v(gamma, e1, TVar(tau')) @
					v(gamma, e2, TVar(tau')) @
					[(tau, TBool)]
				| M.AND -> 
					[(tau, TBool)] @ v(gamma, e1, TBool) @ v(gamma, e2, TBool)
				| M.OR ->
					[(tau, TBool)] @ v(gamma, e1, TBool) @ v(gamma, e2, TBool)
			)

		| M.READ -> [(tau, TInt)]

		| M.WRITE e -> 
				let _ = type_constraint := 
					(!type_constraint)@[(tau, IBS)] in
			v(gamma, e, tau)
		
		| M.MALLOC e ->
			let tau1 = new_var() in
			v(gamma, e, TVar(tau1)) @
			[(tau, TLoc(TVar(tau1)))]
		
		| M.ASSIGN (e1, e2) ->
			v(gamma, e1, TLoc(tau)) @
			v(gamma, e2, tau)

		| M.BANG e ->
			v(gamma, e, TLoc(tau))

		| M.SEQ (e1, e2) ->
			let tau1 = new_var() in
			v(gamma, e, TVar(tau1)) @
			v(gamma, e, tau)
			
		| M.PAIR (e1, e2) ->
			let tau1 = new_var() in
			let tau2 = new_var() in
			[(tau, TPair(TVar(tau1), TVar(tau2)))] @
			v(gamma, e1, TVar(tau1)) @
			v(gamma, e2, TVar(tau2))
		
		| M.FST e ->
			let tau1 = new_var() in
			let tau2 = new_var() in
			[(TVar(tau1), TPair(tau, TVar(tau2)))] @
			v(gamma, e, TVar(tau1))

		| M.SND e ->
			let tau1 = new_var() in
			let tau2 = new_var() in
			[(TVar(tau1), TPair(TVar(tau2), tau))] @
			v(gamma, e, TVar(tau1))

		in

	let init_gamma =  fun x -> raise UnbindError in 
	let init_tau = new_var() in
	let equations = v(init_gamma, exp, TVar(init_tau)) in
	
	let g_decl = ref (fun x ->  0) in
	let g_ans = ref (fun x -> raise UnbindError) in

	let declare (decl, ans, v, t) = 
		let decl_temp = fun x -> if x=v then 1 else (decl x) in
		let ans_temp = fun x -> if x=v then t else (ans x) in 
		(decl_temp, ans_temp)
	in


	(* SOLVE EQUATIONS *)
	let rec iterate (eq, decl, ans) = 
		if (List.length eq)=0 then []
		else 
			(* TVar must be the fst element *)
			(* if tie, then TVar is the fst elmt, and the TTypes is the snd *)
			let (t1, t2) = (List.hd eq) in
			let (t1, t2) =
			match t2 with 
				| TVar v -> (t2, t1)
				| _ -> (t1, t2) in

			match t1 with
				| TInt -> 
				(match t2 with 
			 		| TInt -> iterate((List.tl eq), decl, ans)
			 		| _ -> raise(M.TypeError "Expecting integer")
				)

				| TBool ->
				(match t2 with 
			 		| TBool -> iterate((List.tl eq), decl, ans)
			 		| _ -> raise(M.TypeError "Expecting boolean")
				)

				| TString ->
				(match t2 with 
			 		| TString -> iterate((List.tl eq), decl, ans)
			 		| _ -> raise(M.TypeError "Expecting string")
				)

				| TPair (p1, p2) ->
				(match t2 with
				 	| TPair (p1', p2') -> [(p1, p1');(p2, p2')] @ iterate((List.tl eq), decl, ans)
					| _ -> raise(M.TypeError "Expecting pair")
				)

				| TLoc l ->
				(match t2 with
				 	| TLoc l' -> [(l, l')] @ iterate((List.tl eq), decl, ans)
					| _ -> raise(M.TypeError "Expecting location")
				)

				| TFun (f1, f2) ->
				(match t2 with 
				 	| TFun (f1', f2') -> [(f1, f1');(f2, f2')] @ iterate((List.tl eq), decl, ans)
					| _ -> raise(M.TypeError "Expecting function")  
				)

				| TVar v ->
					(* 1. 1st is not declared yet *)
					if (decl v) = 0 then 
						(match t2 with
						 	(* 1.1 2nd is undeclared variable is not defined *)
						 	| TVar v' ->
								if v = v' then iterate((List.tl eq), decl, ans)
								else if (decl v') = 0 then 
									let (decl', ans') = declare (decl, ans, v, t2) in 
									let _ = g_decl := decl' in
									let _ = g_ans := ans' in
									iterate((List.tl eq), decl', ans')
								(* 1.2 2nd is declared variable, than swap the position *)
								else iterate([(t1, t2)]@(List.tl eq), decl, ans) 
							(* 1.3 2nd is not variable *)
							| _ ->
								let (decl', ans') = declare (decl, ans, v, t2) in 
								let _ = g_decl := decl' in
								let _ = g_ans := ans' in
								iterate((List.tl eq), decl', ans')
						)
					(* 2. 1st is declared *)
					else
						[(ans v, t2)] @ iterate((List.tl eq), decl, ans)
	in

	let rec solve_equation eq = 	
		if (List.length eq) = 0 then ()
		else let e = iterate (eq, !g_decl, !g_ans) in
			solve_equation e in


	
		

	(* debuggin purpose *)
	let rec print_ans vl = 
		if (List.length vl)=0 then ()
		else let v = (List.hd vl) in
			 if (!g_decl v)=1 then 
			 	let _ = print_endline v in
				let _ = print_type (!g_ans v) in (print_ans (List.tl vl))
			else (print_ans (List.tl vl)) in

	let _ = solve_equation equations in
	let _ = print_equ equations in
	
	let _ = print_endline "==ANS==" in
	let _ = print_ans !var_list in
	let _ = print_endline "" in
	raise (M.TypeError "Type Checker Unimplemented")
	(*
  let e' = iterate (equations, !g_decl, !g_ans) in
	let _ = print_endline "==ANS==" in
	let _ = print_ans !var_list in
	let _ = print_endline "" in
	let _ = print_equ e' in
	
	let _ = print_endline "" in
  let e' = iterate (e', !g_decl, !g_ans) in
	let _ = print_endline "==ANS==" in
	let _ = print_ans !var_list in
	let _ = print_endline "" in
	let _ = print_equ e' in
	let _ = print_endline "" in
  let e' = iterate (e', !g_decl, !g_ans) in
	let _ = print_endline "==ANS==" in
	let _ = print_ans !var_list in
	let _ = print_endline "" in
	let _ = print_equ e' in
	let _ = print_endline "" in
  let e' = iterate (e', !g_decl, !g_ans) in
	let _ = print_endline "==ANS==" in
	let _ = print_ans !var_list in
	let _ = print_endline "" in
	let _ = print_equ e' in
	let _ = print_endline "" in
  let e' = iterate (e', !g_decl, !g_ans) in
	let _ = print_endline "==ANS==" in
	let _ = print_ans !var_list in
	let _ = print_endline "" in
	let _ = print_equ e' in
	let _ = print_endline "" in
  let e' = iterate (e', !g_decl, !g_ans) in
	let _ = print_endline "==ANS==" in
	let _ = print_ans !var_list in
	let _ = print_endline "" in
	let _ = print_equ e' in
	let _ = print_endline "" in
  let e' = iterate (e', !g_decl, !g_ans) in
	let _ = print_endline "==ANS==" in
	let _ = print_ans !var_list in
	let _ = print_endline "" in
	let _ = print_equ e' in
	raise (M.TypeError "Type Checker Unimplemented")
	*)

