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

type var = string

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
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

(* id to variable *)
type glist = M.id -> string
type equ = typ * typ

(* TODO : Implement this function *)
let check : M.exp -> M.types = fun exp ->
	(* bind x|->tau at gamma *)
	let bind (gamma, x, tau) = fun t -> if t=x then tau else (gamma x) in

	let rec v : glist * M.exp * typ -> equ list = fun (gamma, e, tau) ->
		match e with
		
		| M.CONST c -> 
			(match c with 
				| M.S s -> [(tau, TString)]
				| M.N n -> [(tau, TInt)] 
				| M.B b -> [(tau, TBool)] 
			)
		
		| M.VAR x -> [(tau, TVar(gamma x))]
		
		| M.FN (x, exp) -> 
			let tau1 = new_var() in
			let tau2 = new_var() in
			let new_gamma = bind (gamma, x, tau1) in
			[(tau, TFun(TVar(tau1), TVar(tau2)))] @ (* t = t1->t2 *)
			(v(new_gamma, exp, TVar(tau2))) (* x : a1 then exp must be a2 *)

		| M.APP (e1, e2) ->
			let tau1 = new_var() in
			v(gamma, e1, TFun(TVar(tau1), tau)) @
			v(gamma, e2, TVar(tau1))
		
		| M.LET (d, e2) ->
			(match d with
			| M.REC (f, x, e1) -> [] (* TODO *)
			| M.VAL (x, e1) -> 
				let tau1 = new_var() in
				let new_gamma = bind (gamma, x, tau1) in
				v(gamma, e1, TVar(tau1)) @
				v(new_gamma, e2, tau)
			)

		| M.IF (e1, e2, e3) ->
			v(gamma, e1, TBool) @
			v(gamma, e2, tau) @
			v(gamma, e3, tau)

		| M.BOP (op, e1, e2) -> [] (* TODO *)

		| M.READ -> [(tau, TInt)]

		| M.WRITE e -> [] (* TODO *)
		
		| M.MALLOC e ->
			let tau1 = new_var() in
			v(gamma, e, TVar(tau1)) @
			[(tau, TLoc(TVar(tau1)))]
		
		| M.ASSIGN (e1, e2) ->
			v(gamma, e1, TLoc(tau)) @
			v(gamma, e2, tau)

		| M.BANG e ->
			let tau1 = new_var() in
			v(gamma, e, TVar(tau1))

		| M.SEQ (e1, e2) ->
			let tau1 = new_var() in
			let tau2 = new_var() in
			v(gamma, e, TVar(tau1)) @
			v(gamma, e, TVar(tau2))
			
		| M.PAIR (e1, e2) ->
			let tau1 = new_var() in
			let tau2 = new_var() in
			[(tau, TPair(TVar(tau1), TVar(tau2)))] @
			v(gamma, e1, TVar(tau1)) @
			v(gamma, e2, TVar(tau2))



 		| _ -> failwith "Unimplemented" 
		in

	let equations = [] in

  raise (M.TypeError "Type Checker Unimplemented")
