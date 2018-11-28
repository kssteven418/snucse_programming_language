
(* multiple encapsuled parameter *)

let val f2 = fn y => (!(!((!y).1))) (!((!y).2)) in

let val f = fn x=>x in
let val fm = malloc f in
let val fmm = malloc fm in
let val f2m = malloc f2 in
let val tup = (fmm, malloc (write true)) in 
(!f2m) (malloc tup)
end
end
end
end
end
end


(* bool *)
