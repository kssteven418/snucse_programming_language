(*
 * SNU 4190.310 Programming Languages 
 * K-- to SM5 translator skeleton code
 *)

open K
open Sm5
module Translator = struct
(*type record
  type loc 
  type value = Z of int | B of bool | L of loc | Unit | R of record
  type cmd = 
    | PUSH of obj 
    | POP 
    | STORE 
    | LOAD 
    | JTR of command * command
    | MALLOC 
    | BOX of int 
    | UNBOX of string 
    | BIND of string 
    | UNBIND
    | GET 
    | PUT 
    | CALL 
    | ADD 
    | SUB 
    | MUL 
    | DIV
    | EQ
    | LESS
    | NOT
  and obj = Val of value | Id of string | Fn of string * command
  and command = cmd list
*)
(*
  type exp =
  | NUM of int | TRUE | FALSE | UNIT
  | VAR of id
  | ADD of exp * exp
  | SUB of exp * exp
  | MUL of exp * exp
  | DIV of exp * exp
  | EQUAL of exp * exp
  | LESS of exp * exp
  | NOT of exp
  | ASSIGN of id * exp          (* assgin to variable *)
  | SEQ of exp * exp            (* sequence *)
  | IF of exp * exp * exp       (* if-then-else *)
  | WHILE of exp * exp          (* while loop *)
  | FOR of id * exp * exp * exp (* for loop *)
  | LETV of id * exp * exp      (* variable binding *)
  | LETF of id * id * exp * exp (* procedure binding *)
  | CALLV of id * exp           (* call by value *)
  | CALLR of id * id            (* call by referenece *)
  | READ of id
  | WRITE of exp
 *)

  (* TODO : complete this function  *)
  let rec trans : K.program -> Sm5.command = function
    
	(* BASICS *)
	(* all debugged *)
  	| K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
	| K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
	| K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
	| K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
	| K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]	
	
	(* NUMERIC OPERATORS *)
	(* all debugged *)
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
	
	(* BOOLEAN OPERATORS *)
	(* all debugged *)
	| K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
	| K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
	| K.NOT e -> trans e @ [Sm5.NOT]
	
	(* ASSIGN OPERATOR *)
	(* all debugged *)
	| K.ASSIGN (x, e) -> trans e @ 
		[Sm5.PUSH (Sm5.Id x); Sm5.STORE] @ (* store *)
		[Sm5.PUSH (Sm5.Id x); Sm5.LOAD] (* restore 'v' on the stack top *)

	(* CONTROL STATEMENTS *)
	| K.SEQ (e1, e2) -> trans e1 @ [Sm5.POP] @ trans e2
	| K.IF (ctrl, et, ef) -> trans ctrl @ [Sm5.JTR(trans et, trans ef)]

	| K.WHILE (ctrl, e) -> 
		let ftnbody = K.IF(K.VAR "v#", K.SEQ(e, K.CALLV("f#", ctrl)), K.UNIT) in
		let whilefun = K.LETF("f#", "v#", ftnbody, K.CALLV("f#", ctrl)) in
		trans whilefun

	| K.FOR (x, el, eh, e) -> 
		let init_ctrl = K.LESS(K.VAR("n2#"), K.VAR("n1#")) in
		let ctrl = K.NOT(K.LESS(K.SUB(K.VAR("n2#"), K.VAR("n1#")), K.VAR("n#"))) in
		let body = K.SEQ(K.SEQ(K.ASSIGN(x, K.ADD(K.VAR("n1#"), K.VAR("n#"))), e), 
					K.ASSIGN("n#", K.ADD(K.VAR("n#"), K.NUM 1))) in
		let forfun = K.LETV("n1#", el,
				K.LETV("n2#", eh,
				K.LETV("n#", K.NUM 0,
				K.IF(init_ctrl, K.UNIT, (* FORF *)
					K.SEQ(K.ASSIGN (x, K.VAR "n1#"), K.WHILE(ctrl, body)))))) in 

		trans forfun

	(* LET STATEMENTS *)
    | K.LETV (x, e1, e2) ->
    	trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
    	trans e2 @ [Sm5.UNBIND; Sm5.POP]
	
	| K.LETF (f, x, e1, e2) -> (* e1 : ftn body *)
		[Sm5.PUSH (Sm5.Fn(x, [Sm5.BIND f] @ trans e1)); 
		(* then stack top : (x, [bind, e1], Env) *)
		(* the function to bind to id f will be provided by call function *)
		Sm5.BIND f] @ trans e2 @
		[Sm5.UNBIND; Sm5.POP]

	(* CALL STATEMENTS *)
	
	| K.CALLV (f, e) -> [Sm5.PUSH (Sm5.Id f)] @ 
		(* it will bind with the id f in the function definition*)
		[Sm5.PUSH (Sm5.Id f)] @ trans e @ [Sm5.MALLOC; Sm5.CALL]

	| K.CALLR (f, x) -> [Sm5.PUSH (Sm5.Id f)] @
		(* it will bind with the id f in the function definition*)
		[Sm5.PUSH (Sm5.Id f) ; (* push function *)
		Sm5.PUSH (Sm5.Id x); Sm5.LOAD; (* push value of the location x *)
		Sm5.PUSH (Sm5.Id x); (* push location x *)
		Sm5.CALL] (* will overwrite the location x with its original value *)

	(* INPUT OUTPUT *)
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
	| K.WRITE e -> trans e @
		[Sm5.MALLOC; Sm5.BIND "loc#"; 
		Sm5.PUSH (Sm5.Id "loc#"); Sm5.STORE] @ (* store the stack top *)
		(* load the stack top twice*)
		(* one is for putting(and will be removed), and the other is to restore the stack top *)
		[Sm5.PUSH (Sm5.Id "loc#"); Sm5.LOAD; 
		Sm5.PUSH (Sm5.Id "loc#"); Sm5.LOAD] @ 
		[Sm5.PUT; Sm5.UNBIND] (* Write -> stack top poped out *)

end
