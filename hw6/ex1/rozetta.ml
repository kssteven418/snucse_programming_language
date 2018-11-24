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
	(* make caller calls callee and callee calls caller *)
  | Sm5.Fn (arg, command) -> 
		let new_cmd = 
			(* first, store the caller function *)
			[Sonata.BIND "!caller_ftn"] @
			(* Now the stack is S, so execute the function command *)
			trans' command @
			(* restore the caller function and unbind the temporary location *)
			[Sonata.PUSH (Sonata.Id "!caller_ftn") ; Sonata.UNBIND ; Sonata.POP] @
			(* push a garbage value *)
			[Sonata.PUSH (Sonata.Val (Sonata.Unit))] @
			(* push a garbage location *)
			[Sonata.MALLOC; Sonata.CALL] in
			(* call the caller function *)
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
	(* this caller function must be called
	   at the end of the callee function
		 to return to the 'cmds' *)
	let caller_ftn = Sonata.Fn("!arg", trans' cmds) in (* returning to the caller *)
	(* store the stack top v and proc.
	   don't need to store l, since a new location will be constructed *)
	let new_cmd =  
		[Sonata.MALLOC ; Sonata.BIND "!l" ; Sonata.PUSH(Sonata.Id "!l") ; Sonata.STORE;
		Sonata.MALLOC ; Sonata.BIND "!v" ; Sonata.PUSH(Sonata.Id "!v") ; Sonata.STORE;
		Sonata.MALLOC ; Sonata.BIND "!f" ; Sonata.PUSH(Sonata.Id "!f") ; Sonata.STORE] @	
		(* pass the caller_ftn to the callee through the stack *)
		[Sonata.PUSH caller_ftn] @
		(* restore the proc and v *)
		[Sonata.PUSH (Sonata.Id "!f") ; Sonata.LOAD;
		Sonata.PUSH (Sonata.Id "!v") ; Sonata.LOAD; 
		Sonata.PUSH (Sonata.Id "!l") ; Sonata.LOAD] @
		(* unbind the temporary locations *)
		[Sonata.UNBIND ; Sonata.POP ; Sonata.UNBIND ; Sonata.POP] @
		[Sonata.UNBIND ; Sonata.POP ] @
		(* Assign a new location for the parameter passing *)
		(*[Sonata.MALLOC] @*)
		(* Now, the stack top will be new_loc :: param :: callee_ftn *)
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
