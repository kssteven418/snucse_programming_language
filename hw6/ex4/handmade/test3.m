(* function type unification *)

let val f1 = fn x => x+1 in
let val f2 = fn y => f1 1 in
	false = ((write 1) = (write (f1 read)))
end
end

(* result : Bool *)
