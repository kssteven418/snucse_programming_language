let proc f(x) = x+1 in
let proc g(x) = x-1 in
let proc incr(x, y) =
( x := x+1;
  y := y-1;
  if(0<y) then incr<x, y> else 100) in
let z := {x:=1, y:=1} in
let x := 10 in
let y := 5 in
let temp := 0 in
write incr<x, y>;
write x;
write y;
1200
