
(* rec *)

let rec f = fn x => 
	if true then (f x, f x )
	else f x in
f 10
end

(* type error *)
