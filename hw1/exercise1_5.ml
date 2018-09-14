
(* 2015-18525 Sehoon Kim *)

(* exercise 5 *)

type nat = ZERO | SUCC of nat 

let rec natadd : nat * nat -> nat = fun (x, y) ->
	match x with
	|ZERO -> y
	|SUCC s -> SUCC(natadd (s,y))

let rec natmul : nat * nat -> nat = fun (x, y) ->
	match x with 
	|ZERO -> ZERO
	|SUCC s -> natadd (natmul (s, y), y)

(* debugging 5 

let rec nat_to_int : nat -> int = fun x ->
	match x with
	|ZERO -> 0
	|SUCC x -> nat_to_int(x) + 1

let rec nat_to_string : nat -> string = fun x ->
	match x with
	|ZERO -> "ZERO"
	|SUCC x -> "SUCC(" ^ (nat_to_string x) ^ ")"


let n4 = SUCC(SUCC(SUCC(SUCC(ZERO))))
let n2 = SUCC(SUCC(ZERO))
let n1 = SUCC(ZERO)
let n0 = ZERO

*)

(* debug natadd

let n0_0 = natadd (n0, n0)
let n0_1 = natadd (n0, n1)
let n0_2 = natadd (n0, n2)
let n1_2 = natadd (n1, n2)
let n2_4 = natadd (n2, n4)
let n4_4 = natadd (n4, n4)

let n0_0 = natadd (n0, n0)
let n0_1 = natadd (n1, n0)
let n0_2 = natadd (n2, n0)
let n1_2 = natadd (n2, n1)
let n2_4 = natadd (n4, n2)
let n4_4 = natadd (n4, n4)

let _ = print_endline (nat_to_string n0_0)
let _ = print_endline (nat_to_string n0_1)
let _ = print_endline (nat_to_string n0_2)
let _ = print_endline (nat_to_string n1_2)
let _ = print_endline (nat_to_string n2_4)
let _ = print_endline (nat_to_string n4_4)

*)

(* debug natmul

let n0_0 = natmul (n0, n0)
let n0_1 = natmul (n0, n1)
let n0_2 = natmul (n0, n2)
let n1_2 = natmul (n1, n2)
let n2_4 = natmul (n2, n4)
let n4_4 = natmul (n4, n4)

let n0_0 = natmul (n0, n0)
let n0_1 = natmul (n1, n0)
let n0_2 = natmul (n2, n0)
let n1_2 = natmul (n2, n1)
let n2_4 = natmul (n4, n2)
let n4_4 = natmul (n4, n4)

let i0_0 = nat_to_int(n0_0)
let i0_1 = nat_to_int(n0_1)
let i0_2 = nat_to_int(n0_2)
let i1_2 = nat_to_int(n1_2)
let i2_4 = nat_to_int(n2_4)
let i4_4 = nat_to_int(n4_4)

let _ = print_endline (string_of_int i0_0)
let _ = print_endline (string_of_int i0_1)
let _ = print_endline (string_of_int i0_2)
let _ = print_endline (string_of_int i1_2)
let _ = print_endline (string_of_int i2_4)
let _ = print_endline (string_of_int i4_4)

let _ = print_endline (nat_to_string (natmul (SUCC (SUCC (SUCC (SUCC ZERO))), SUCC (SUCC (SUCC ZERO)))))

*)
