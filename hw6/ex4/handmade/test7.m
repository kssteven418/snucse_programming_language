
let val f1 = fn x => x+1 in
let val f2 = fn y => y 10 in
	write (f1 1) = (f2 f1)
end
end
(* bool *)
