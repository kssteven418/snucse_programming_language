(*
 * SNU 4190.310 Programming Languages 
 * Homework "Rozetta" Skeleton
 *)

let trans_v : Sm5.value -> Sonata.value = function
  | Sm5.Z z  -> Sonata.Z z
  | Sm5.B b  -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

let rec trans_obj : Sm5.obj -> Sonata.obj = function
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  (* TODO *)
	(* make sure that the callee function calls its caller at the end of the procedure 
	   in order to return back to the caller side *)
  | Sm5.Fn (arg, command) -> 
		let new_cmd = trans' (
				(* right after the function call, the stack top is a caller_ftn,
				   -> store this at the environment *)
				[Sm5.BIND "!caller_ftn" ] @
				(* execute the function body *)
				command @
				(* restore the caller_ftn *)
				[Sm5.PUSH (Sm5.Id "!caller_ftn")] @
				(* unbind unnecessary environment *)
				[Sm5.UNBIND; Sm5.POP] @
				(* push a trash value *)
				[Sm5.PUSH (Sm5.Val Sm5.Unit)] @	
				(* push a trash location *)
				[Sm5.MALLOC] @

				(* stack top will be trash_loc :: trash_val :: caller_ftn 
				   caller_ftn is a simple return statement, so parameter passing is not required
					 -> hence using trash location and value *)

				(* this will simply translated to Sonata.CALL *)
				[Sm5.CALL] ) in
		Sonata.Fn (arg, new_cmd)

(* TODO : complete this function *)
and trans' : Sm5.command -> Sonata.command = function
  | Sm5.PUSH obj :: cmds -> Sonata.PUSH (trans_obj obj) :: (trans' cmds)
  | Sm5.POP :: cmds -> Sonata.POP :: (trans' cmds)
  | Sm5.STORE :: cmds -> Sonata.STORE :: (trans' cmds)
  | Sm5.LOAD :: cmds -> Sonata.LOAD :: (trans' cmds)

  (* TODO : JTR *)
  | Sm5.JTR (c1, c2) :: cmds ->
  	[Sonata.JTR (trans' (c1@cmds), trans' (c2@cmds))]

  | Sm5.MALLOC :: cmds -> Sonata.MALLOC :: (trans' cmds)
  | Sm5.BOX z :: cmds -> Sonata.BOX z :: (trans' cmds)
  | Sm5.UNBOX id :: cmds -> Sonata.UNBOX id :: (trans' cmds)
  | Sm5.BIND id :: cmds -> Sonata.BIND id :: (trans' cmds)
  | Sm5.UNBIND :: cmds -> Sonata.UNBIND :: (trans' cmds)
  | Sm5.GET ::cmds -> Sonata.GET :: (trans' cmds)
  | Sm5.PUT ::cmds -> Sonata.PUT :: (trans' cmds)
	
  (* TODO : CALL *)
  | Sm5.CALL :: cmds -> 
	(* if the CALL function is at the end of the commands,
	   then, does not need to come back to after the CALL.
		 We simply end up the procedure at the callee part, w/o returning to the caller part.
		 -> similar to "goto statement" *)
	(* this will work same as return statement of functions *)
	if cmds=[] then [Sonata.CALL]
	
	(* else, we do need to come back to the caller part after the CALL *)
	else
	(* this caller_ftn must be called at the end of the callee function
		 to return bacj to the caller and execute the remaining commands (cmds) *)

	(* unbind !arg, !f, !v, and !l *)
	let pop_cmd = 
		[Sonata.UNBIND ; Sonata.POP;
		Sonata.UNBIND ; Sonata.POP;
		Sonata.UNBIND ; Sonata.POP;
		Sonata.UNBIND ; Sonata.POP] in
	let caller_ftn = Sonata.Fn("!arg", pop_cmd @ (trans' cmds)) in (* returning to the caller *)
	let new_cmd =  
		(* store the stack top l, v and proc. *)
		[Sonata.MALLOC ; Sonata.BIND "!l" ; Sonata.PUSH(Sonata.Id "!l") ; Sonata.STORE;
		Sonata.MALLOC ; Sonata.BIND "!v" ; Sonata.PUSH(Sonata.Id "!v") ; Sonata.STORE;
		Sonata.MALLOC ; Sonata.BIND "!f" ; Sonata.PUSH(Sonata.Id "!f") ; Sonata.STORE] @	
		(* pass the caller_ftn to the callee through the stack *)
		[Sonata.PUSH caller_ftn] @
		(* restore the proc, v and l *)
		[Sonata.PUSH (Sonata.Id "!f") ; Sonata.LOAD;
		Sonata.PUSH (Sonata.Id "!v") ; Sonata.LOAD; 
		Sonata.PUSH (Sonata.Id "!l") ; Sonata.LOAD] @
		(* unbind the temporary locations *)
		[Sonata.UNBIND ; Sonata.POP ; Sonata.UNBIND ; Sonata.POP] @
		[Sonata.UNBIND ; Sonata.POP ] @
		(* Now, the stack top will be l :: v :: f :: caller_ftn *)
		[Sonata.CALL] in
	new_cmd

  | Sm5.ADD :: cmds -> Sonata.ADD :: (trans' cmds)
  | Sm5.SUB :: cmds -> Sonata.SUB :: (trans' cmds)
  | Sm5.MUL :: cmds -> Sonata.MUL :: (trans' cmds)
  | Sm5.DIV :: cmds -> Sonata.DIV :: (trans' cmds)
  | Sm5.EQ :: cmds -> Sonata.EQ :: (trans' cmds)
  | Sm5.LESS :: cmds -> Sonata.LESS :: (trans' cmds)
  | Sm5.NOT :: cmds -> Sonata.NOT :: (trans' cmds)
  | [] -> []

(* TODO : complete this function *)
let trans : Sm5.command -> Sonata.command = fun command ->
	trans' command

