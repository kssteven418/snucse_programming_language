(* locations vs. location of location *)

let val temp = malloc true in
let val temp2 = malloc temp in
let val temp3 = malloc temp in
	temp = temp3
end
end
end

(* type error *)
