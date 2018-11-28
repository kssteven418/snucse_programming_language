
let val f1 = fn x => x+1 in
let val f2 = fn y => y 1 in
let val ff = (f1, f2) in

(ff.2) (ff.1)

end
end
end

(* int *)
