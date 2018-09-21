
(* 2015-18525 Sehoon Kim *)
(* exercise 2-2 *)

exception InvalidArgument

type ae = CONST of int
	| VAR of string
	| POWER of string * int
	| TIMES of ae list
	| SUM of ae list


let rec diff : ae * string -> ae = fun ( a, var ) ->
	match a with
	| CONST n -> CONST 0
	
	| VAR x ->
		if x = var then CONST 1
		else CONST 0
	
	| POWER (base, exp) ->(* base ^ exp *)
		if not (base = var) then CONST 0 (* if not related variable *)
		else if exp == 0 then CONST 0 (* x^0 == CONST 1 *)
		else if exp == 1 then CONST 1 (* x^1 == VAR x *)
		else TIMES([CONST exp; POWER(base, exp-1)])
	
	| SUM a_list ->
		if List.length a_list == 0 then raise InvalidArgument
		else let rec diff_list l =
			if List.length l == 0 then []
			else [diff ((List.hd l), var)] @ (diff_list (List.tl l)) in
			SUM(diff_list a_list)			
	
	| TIMES a_list ->
		if List.length a_list ==0 then raise InvalidArgument
		else let rec diff_list (l, l_len) =
			if l_len == 0 then []
			else [TIMES([diff ((List.hd l), var)] @ (List.tl l))] 
				@ (diff_list (((List.tl l) @ [List.hd l]), l_len-1)) in
			SUM(diff_list (a_list, (List.length a_list)))




(* debugging *)
(*
let _ = print_endline "testing power"

let ae3 = POWER("x", 2)
let ae4 = POWER("x", 1)
let ae5 = POWER("x", 0)
let ae6 = POWER("x", -1)

let _ = diff(ae3, "y")
let _ = diff(ae4, "y")
let _ = diff(ae5, "y")
let _ = diff(ae6, "y")

let _ = diff(ae3, "x")
let _ = diff(ae4, "x")
let _ = diff(ae5, "x")
let _ = diff(ae6, "x")
*)

(*

let _ = print_endline "testing sum"
let ae2 = SUM([CONST 0; VAR "x"; VAR "y"; POWER("x", 2); POWER("x", 1); POWER("x", 0); POWER("y", 2)])

let _ = print_endline (print_ae ae2)
let _ = print_endline (print_ae (diff (ae2, "x")))
let _ = print_endline (print_ae (diff (ae2, "y")))
let _ = print_endline (print_ae (diff (ae2, "z")))

let ae1 = SUM([SUM([POWER("x", 2); POWER("x", 1); POWER("x", 0); POWER("y", 2)]); SUM([POWER("y", 4); POWER("x", -4); CONST 1; VAR "x"]); POWER("x", -3)])

let _ = print_endline (print_ae ae1)
let _ = print_endline (print_ae (diff (ae1, "x")))
let _ = print_endline (print_ae (diff (ae1, "y")))

let exp =TIMES([CONST 5; VAR "y";  TIMES([SUM([VAR "x"; CONST 1]);VAR "x"])])
let _ = diff(exp, "x") 
*)
