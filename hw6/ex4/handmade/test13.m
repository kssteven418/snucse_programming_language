(* passing pair to parameter *)

let val f2 = fn y => (y.1) (y.2) in
let val f = fn x=>x in
f2 (f, 1) 
end
end

