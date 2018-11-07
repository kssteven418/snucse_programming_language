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
  	| K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
	| K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
	| K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
	| K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
	| K.VAR x -> [Sm5.PUSH (Sm5.Id x); Sm5.LOAD]	
	
	(* NUMERIC OPERATORS *)
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
	
	(* BOOLEAN OPERATORS *)
	| K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
	| K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
	| K.NOT e -> trans e @ [Sm5.NOT]

	(* CONTROL STATEMENTS *)
	| SEQ (e1, e2) -> trans e1 @ trans e2

    | K.LETV (x, e1, e2) ->
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]

	(* INPUT OUTPUT *)
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
	| K.WRITE e -> trans e @ [Sm5.PUT]
    | _ -> failwith "Unimplemented"

end
