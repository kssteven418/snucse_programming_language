
(* 2015-18525 Sehoon Kim *)


(* excercise 3 *)

type team = Korea | France | Usa | Brazil 
		  | Japan | Nigeria | Cameroon | Poland 
		  | Portugal | Italy | Germany | Norway 
		  | Sweden | England | Argentina

type tourna = LEAF of team | NODE of tourna * tourna

let rec parenize : tourna -> string = fun t -> 
	match t with
	|NODE (left, right) -> "(" ^ (parenize left) ^ " " ^ (parenize right) ^ ")"
	|LEAF leaf -> 
		(match leaf with
		 |Korea -> "Korea"
		 |France -> "France"
		 |Usa -> "Usa"
		 |Brazil -> "Brazil"
		 |Japan -> "Japan"
		 |Nigeria -> "Nigeria"
		 |Cameroon -> "Cameroon"
		 |Poland -> "Poland"
		 |Portugal -> "Portugal"
		 |Italy -> "Italy"
		 |Germany -> "Germany"
		 |Norway -> "Norway"
		 |Sweden -> "Sweden"
		 |England -> "England"
		 |Argentina -> "Argentina")


(* debugging 3

let t_temp = NODE(NODE(LEAF Korea, LEAF Portugal), LEAF Brazil)
let s_temp = parenize(t_temp)
let _ = print_endline s_temp

let _ = print_endline (parenize (NODE(LEAF Norway, NODE(NODE(LEAF Cameroon, LEAF Poland), LEAF Sweden))))

let _ = print_endline (parenize (LEAF Korea))
let _ = print_endline (parenize (NODE(LEAF Korea, LEAF Japan)))
let _ = print_endline (parenize (NODE(NODE(LEAF Korea, LEAF Japan), LEAF Japan)))

*)
