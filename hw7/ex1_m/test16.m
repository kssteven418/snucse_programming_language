(* Polymorphism with WRITE (type error) *)

	(*
let val print = fn x => 
  (write x; true) 
in
  (print 1, (print (fn x => x), print true))
end
*)

let val print = fn x => (write x; true)
in
print(fn x => x)
	end

(* Result : type error *)
