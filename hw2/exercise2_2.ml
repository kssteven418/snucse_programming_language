

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



let rec print_ae : ae -> string = fun a ->
	match a with
	| CONST n -> "CONST " ^ (string_of_int n)
	| VAR x -> "VAR " ^ x
	| POWER (base, exp) -> "POWER (" ^ base ^ ", " ^ (string_of_int exp) ^")"
	| SUM a_list -> 
		let rec iter l =
			if List.length l == 1 then (print_ae (List.hd l))
			else (print_ae (List.hd l)) ^ ", " ^ (iter (List.tl l)) in
			"SUM (" ^ (iter a_list) ^ ")"
	| TIMES a_list -> 
		let rec iter l =
			if List.length l == 1 then (print_ae (List.hd l))
			else (print_ae (List.hd l)) ^ ", " ^ (iter (List.tl l)) in
			"TIMES (" ^ (iter a_list) ^ ")"


(* debugging *)

(*
let _ = print_endline "testing power"

let ae3 = POWER("x", 2)
let ae4 = POWER("x", 1)
let ae5 = POWER("x", 0)
let ae6 = POWER("x", -1)
let _ = print_endline (print_ae (diff (ae3, "x")))
let _ = print_endline (print_ae (diff (ae4, "x")))
let _ = print_endline (print_ae (diff (ae5, "x")))
let _ = print_endline (print_ae (diff (ae6, "x")))
let _ = print_endline (print_ae (diff (ae3, "y")))


let _ = print_endline "testing var and const"

let ae7 = VAR "x"
let ae8 = CONST 1
let _ = print_endline (print_ae (diff (ae7, "x")))
let _ = print_endline (print_ae (diff (ae7, "y")))
let _ = print_endline (print_ae (diff (ae8, "x")))
let _ = print_endline (print_ae (diff (ae8, "y")))


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
*)


let _ = print_endline "testing times"
let ae1 = TIMES([TIMES([VAR "x"; VAR "y"]); SUM([POWER("x", 2); POWER("x", 1); POWER("y", -2)]); POWER("x", 2)])

let _ = print_endline (print_ae ae1)
let _ = print_endline (print_ae (diff (ae1, "x")))
let _ = print_endline (print_ae (diff (ae1, "y")))
(*
SUM (
	TIMES (
		SUM (
			TIMES (CONST 1, VAR y), 
			TIMES (CONST 0, VAR x)), 
		SUM (POWER (x, 2), POWER (x, 1), POWER (y, -2)), 
		POWER (x, 2)), 
	TIMES (
		SUM (
			TIMES (CONST 2, POWER (x, 1)), 
			CONST 1, 
			CONST 0), 
		POWER (x, 2), 
		TIMES (VAR x, VAR y)), 
	TIMES (
		TIMES (CONST 2, POWER (x, 1)), 
		TIMES (VAR x, VAR y), 
		SUM (POWER (x, 2), POWER (x, 1), POWER (y, -2))))
*)
