
(* nested function *)

let val h = fn f =>
	f 10 20 in
let val f = fn x => 
	let val g = fn y => x+y in 
	g 
	end in
h f
end
end
