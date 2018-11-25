(* Public testcase 1 : Addition *)

1 + 2 + ((fn f => f + 3) 0)
(*
(fn f => f) 0
*)
(* Output : 6 *)
