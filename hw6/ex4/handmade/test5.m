
(* function type unification *)

let val f1 = fn x => x+1 in
let val f2 = fn y => f1 1 in
	write (1, 1)
end
end

(* result : Typerror *)
