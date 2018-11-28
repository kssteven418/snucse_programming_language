
(*
let val f = fn ff => 
	let val x = ff.1 in
	let val f3 = ff.2 in
	let val f1 = f3.1 in
	let val f2 = f3.2 in
	f2 (f1, x)
	end
	end
	end
	end in
	*)
let val f2 = fn y => (y.1) (y.2) in
f2
end

