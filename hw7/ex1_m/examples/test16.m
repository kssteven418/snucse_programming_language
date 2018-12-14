(* Polymorphism with WRITE (type error) *)

let val print = fn x => 
  (write x; true) 
in
  ( (print (fn x => x), print true))
end

(* Result : type error *)
