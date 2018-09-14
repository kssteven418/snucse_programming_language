
(* 2015-18525 Sehoon Kim *)

(* exercise 1 *)

let rec sigma : int * int * (int->int) -> int = fun (a, b, f) ->
	if a==b then (f a)
	else if a > b then 0
	else (f a) + (sigma (a+1, b, f))

		
(* debugging 1 

let double = fun x -> x * 2
let square = fun x -> x * x
let plus1 = fun x -> x+1

let a = 100
let b = 101

let s1 = sigma (a, b, double)
let s2 = sigma (a, b, square)
let s3 = sigma (a, b, plus1)

let _ = print_endline (string_of_int s1)
let _ = print_endline (string_of_int s2)
let _ = print_endline (string_of_int s3)

let _ = print_endline (string_of_int((sigma (1, 10, fun x -> x))))
*)
