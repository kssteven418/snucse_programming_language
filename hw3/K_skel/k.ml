(*
 * SNU 4190.310 Programming Languages 2018 Fall
 *  K- Interpreter Skeleton Code
 *)

(* Location Signature *)
module type LOC =
sig
  type t
  val base : t
  val equal : t -> t -> bool
  val diff : t -> t -> int
  val increase : t -> int -> t
end

module Loc : LOC =
struct
  type t = Location of int
  let base = Location(0)
  let equal (Location(a)) (Location(b)) = (a = b)
  let diff (Location(a)) (Location(b)) = a - b
  let increase (Location(base)) n = Location(base+n)
end

(* Memory Signature *)
module type MEM = 
sig
  type 'a t
  exception Not_allocated
  exception Not_initialized
  val empty : 'a t (* get empty memory *)
  val load : 'a t -> Loc.t  -> 'a (* load value : Mem.load mem loc => value *)
  val store : 'a t -> Loc.t -> 'a -> 'a t (* save value : Mem.store mem loc value => mem' *)
  val alloc : 'a t -> Loc.t * 'a t (* get fresh memory cell : Mem.alloc mem => (loc, mem') *)
end

(* Environment Signature *)
module type ENV =
sig
  type ('a, 'b) t
  exception Not_bound
  val empty : ('a, 'b) t (* get empty environment *)
  val lookup : ('a, 'b) t -> 'a -> 'b (* lookup environment : Env.lookup env key => content *)
  val bind : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t  (* id binding : Env.bind env key content => env'*)
end

(* Memory Implementation *)
module Mem : MEM =
struct
  exception Not_allocated
  exception Not_initialized
  type 'a content = V of 'a | U
  type 'a t = M of Loc.t * 'a content list
  let empty = M (Loc.base,[])

  let rec replace_nth = fun l n c -> 
    match l with
    | h::t -> if n = 1 then c :: t else h :: (replace_nth t (n - 1) c)
    | [] -> raise Not_allocated

  let load (M (boundary,storage)) loc =
    match (List.nth storage ((Loc.diff boundary loc) - 1)) with
    | V v -> v 
    | U -> raise Not_initialized

  let store (M (boundary,storage)) loc content =
    M (boundary, replace_nth storage (Loc.diff boundary loc) (V content))

  let alloc (M (boundary,storage)) = 
    (boundary, M (Loc.increase boundary 1, U :: storage))
end

(* Environment Implementation *)
module Env : ENV=
struct
  exception Not_bound
  type ('a, 'b) t = E of ('a -> 'b)
  let empty = E (fun x -> raise Not_bound)
  let lookup (E (env)) id = env id
  let bind (E (env)) id loc = E (fun x -> if x = id then loc else env x)
end

(*
 * K- Interpreter
 *)
module type KMINUS =
sig
  exception Error of string
  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp
    
  type program = exp
  type memory
  type env
  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
  val emptyMemory : memory
  val emptyEnv : env
  val run : memory * env * program -> value
end

module K : KMINUS =
struct
  exception Error of string

  type id = string
  type exp =
    | NUM of int | TRUE | FALSE | UNIT
    | VAR of id
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | EQUAL of exp * exp
    | LESS of exp * exp
    | NOT of exp
    | SEQ of exp * exp            (* sequence *)
    | IF of exp * exp * exp       (* if-then-else *)
    | WHILE of exp * exp          (* while loop *)
    | LETV of id * exp * exp      (* variable binding *)
    | LETF of id * id list * exp * exp (* procedure binding *)
    | CALLV of id * exp list      (* call by value *)
    | CALLR of id * id list       (* call by referenece *)
    | RECORD of (id * exp) list   (* record construction *)
    | FIELD of exp * id           (* access record field *)
    | ASSIGN of id * exp          (* assgin to variable *)
    | ASSIGNF of exp * id * exp   (* assign to record field *)
    | READ of id
    | WRITE of exp

  type program = exp

  type value =
    | Num of int
    | Bool of bool
    | Unit
    | Record of (id -> Loc.t)
    
  type memory = value Mem.t
  type env = (id, env_entry) Env.t
  and  env_entry = Addr of Loc.t | Proc of id list * exp * env

  let emptyMemory = Mem.empty
  let emptyEnv = Env.empty

  let value_int v =
    match v with
    | Num n -> n
    | _ -> raise (Error "TypeError : not int")

  let value_bool v =
    match v with
    | Bool b -> b
    | _ -> raise (Error "TypeError : not bool")

  let value_unit v =
    match v with
    | Unit -> ()
    | _ -> raise (Error "TypeError : not unit")

  let value_record v =
    match v with
    | Record r -> r
    | _ -> raise (Error "TypeError : not record")

  let lookup_env_loc e x =
    try
      (match Env.lookup e x with
      | Addr l -> l
      | Proc _ -> raise (Error "TypeError : not addr")) 
    with Env.Not_bound -> raise (Error ("Unbound : loc "^x))

  let lookup_env_proc e f =
    try
      (match Env.lookup e f with
      | Addr _ -> raise (Error "TypeError : not proc") 
      | Proc (id_list, exp, env) -> (id_list, exp, env))
    with Env.Not_bound -> raise (Error ("Unbound : proc "^f))


  (* sub function : evaluate integer operation *)
  (* cast type error if either v1 or v2 is not an integer *)
  let eval_int_op (v1, v2, op) = 
		let n1 = value_int v1 in
		let n2 = value_int v2 in
	    if op == 0 then Num(n1+n2)
			else if op == 1 then Num(n1-n2)
			else if op == 2 then Num(n1*n2)
			else Num(n1/n2)


  let rec eval mem env e =
    match e with
    | READ x -> 
      let v = Num (read_int()) in
      let l = lookup_env_loc env x in
      (v, Mem.store mem l v)

    | WRITE e ->
      let (v, mem') = eval mem env e in
      let n = value_int v in
      let _ = print_endline (string_of_int n) in
      (v, mem')

	
		(* TODO *)

		(* basics *)

		| NUM n -> (Num(n), mem)
		| TRUE -> (Bool(true), mem)
		| FALSE -> (Bool(false), mem)
		| UNIT -> (Unit, mem)

		| VAR x ->
			let l = lookup_env_loc env x in
			let v = Mem.load mem l in
			(v, mem)
		
		(* end of basics *)

		(* integer/bool operators *)
		(* let's assume that e1 and e2 do not alter the memory contents *)
		
		| ADD (e1, e2) ->
			let (v1, mem1) = eval mem env e1 in
			let (v2, mem2) = eval mem1 env e2 in
			(eval_int_op(v1, v2, 0), mem2)
	
		| SUB (e1, e2) ->
			let (v1, mem1) = eval mem env e1 in
			let (v2, mem2) = eval mem1 env e2 in
			(eval_int_op(v1, v2, 1), mem2)
	
		| MUL (e1, e2) ->
			let (v1, mem1) = eval mem env e1 in
			let (v2, mem2) = eval mem1 env e2 in
			(eval_int_op(v1, v2, 2), mem2)
	
		| DIV (e1, e2) ->
			let (v1, mem1) = eval mem env e1 in
			let (v2, mem2) = eval mem1 env e2 in
			(eval_int_op(v1, v2, 3), mem2)
	
		| EQUAL (e1, e2) ->
		  let (v1, mem1) = eval mem env e1 in
			let (v2, mem2) = eval mem1 env e2 in
			(match v1 with 
			| Num n1 ->
				(match v2 with 
				 | Num n2 -> 
				 (*let _ = print_endline (string_of_bool (n1 == n2)) in*)
				 (Bool(n1 == n2), mem2)
				 | _ -> 
				 (*let _ = print_endline (string_of_bool (false)) in*)
				 (Bool(false), mem2)
				)
			| Bool b1 ->
				(match v2 with
				 | Bool b2 -> 
				 (*let _ = print_endline (string_of_bool (b1 == b2)) in*)
				 (Bool(b1 == b2), mem2)
				 | _ -> 
				 (*let _ = print_endline (string_of_bool (false)) in*)
				 (Bool(false), mem2)
				)
			| Unit ->
				(match v2 with
				 | Unit -> 
				 (*let _ = print_endline (string_of_bool (true)) in*)
				 (Bool(true), mem2)
				 | _ -> 
				 (*let _ = print_endline (string_of_bool (false)) in*)
				 (Bool(false), mem2)
				)
			| _ -> 
			(*let _ = print_endline (string_of_bool (false)) in*)
			(Bool(false), mem2))
		
		| LESS (e1, e2) ->
			let (v1, mem1) = eval mem env e1 in
			let (v2, mem2) = eval mem1 env e2 in
			let n1 = value_int v1 in
			let n2 = value_int v2 in
			(* let _ = print_endline (string_of_bool (n1 < n2)) in *)
			(Bool(n1 < n2), mem2)

		| NOT e ->
			let (v, mem1) = eval mem env e in
			let b = value_bool v in
			(* let _ = print_endline (string_of_bool (not b)) in *)
			(Bool(not b), mem1)
		
		(* end of integer/bool operators *)
		
		(* control operators *)

		| SEQ (e1, e2) ->
			(* evaluate e1 and then evaluate e2 *)
			(* environment may not be altered... *)
		  let (v1, mem1) = eval mem env e1 in
			let (v2, mem2) = eval mem1 env e2 in
			(v2, mem2)
		
		(* if e1 then e2, else e3 *) 
		| IF (e1, e2, e3) ->
			let (ctrl, mem1) = eval mem env e1 in
			(* execute e2 if ctrl is true *)
			if (value_bool ctrl) then let (v, mem2) = eval mem1 env e2 in 
				(v, mem2)
			(* execute e3 if ctrl is false *)
			else let (v, mem2) = eval mem1 env e3 in
				(v, mem2)
		
		| WHILE (e1, e2) ->
			let (ctrl, mem1) = eval mem env e1 in
			if (value_bool ctrl) then (*if true *)
				let (v1, mem2) = eval mem1 env e2 in (* eval this iteration *)
				(* next iterations *)
				let (v2, mem3) = eval mem2 env (WHILE(e1, e2)) in
				(v2, mem3)
			else (Unit, mem)

		(* end of control operators *)

		(* binding *)

    | LETV (x, e1, e2) ->
      let (v, mem') = eval mem env e1 in (* evaluate e1 -> value v *)
      let (l, mem'') = Mem.alloc mem' in (* new location l *)
      eval (Mem.store mem'' l v) (Env.bind env x (Addr l)) e2
		  (* new memory : store value v at location l *)
		  (* new env : id x to memotry location l *)
		  (* then, recursively call eval function with remaining e2 *)

		| LETF (f, x_list, e1, e2) ->
			let new_env = Env.bind env f (Proc (x_list, e1, env)) in
			eval mem new_env e2

		(* end of binding *)

		(* call by value & ref *)

		| CALLV (f, e_list) ->
			(* inner function : evaluate expression list *)
			let rec eval_list (env0, mem0, e_list0) =
				if (List.length e_list0) == 0 then (mem0, [])
				(* calculate current expression *)
				else let (v, mem1) = eval mem0 env0 (List.hd e_list0) in 
					(* recursively calculate next expressions *)
					let (mem2, v_list) = eval_list(env0, mem1, (List.tl e_list0)) in
					(mem2, [v]@v_list) in

			(* inner function : bind variable list *)
			let rec bind_variables (env, mem, x_list, v_list) = 
				if not ((List.length x_list) == (List.length v_list)) 
					then raise (Error "InvalidArg")
				else if (List.length x_list) == 0 then (env, mem)
				else let (l, mem') = Mem.alloc mem in
					let new_env = (Env.bind env (List.hd x_list) (Addr l)) in (* bind new location to env *)
					let new_mem = (Mem.store mem' l (List.hd v_list)) in (* store value at the new location *)
					bind_variables (new_env, new_mem, (List.tl x_list), (List.tl v_list)) in

			(* evaluate e_list and make value list, v_list *)
			let (mem_n, v_list) = eval_list(env, mem, e_list) in
			(* find procedure with id f *)
			(* exp' : function budy, env' : define-time env. *)
			let (x_list, exp', env') = lookup_env_proc env f in 
			(* bind variables to the function parameter, x_list and modify env *)
			let (env_vb, mem_vb) = bind_variables (env', mem_n, x_list, v_list) in
			(* the bind the function *)
			let env_fb = Env.bind env_vb f (Proc (x_list, exp', env')) in
			eval mem_vb env_fb exp' 

		| CALLR (f, y_list) ->
			(* inner function : bind variables *)
			let rec bind_variables (env0, x_list, y_list) =
				if not ((List.length x_list) == (List.length y_list)) 
					then raise (Error "InvalidArg")
				else if (List.length x_list) == 0 then env0
				else let l = lookup_env_loc env (List.hd y_list) in (* l = location for yi *)
					let new_env = (Env.bind env0 (List.hd x_list) (Addr l)) in (* location for xi <- l *)
					bind_variables (new_env, (List.tl x_list), (List.tl y_list)) in

			(* find procedure with id f *)
			(* exp' : function budy, env' : define-time env. *)
			let (x_list, exp', env') = lookup_env_proc env f in 
			(* bind variables *)
			let env_vb = bind_variables (env', x_list, y_list) in
			let env_fb = Env.bind env_vb f (Proc (x_list, exp', env')) in 
			eval mem env_fb exp'
	
		(* end of call by value/reference *)

		(* record *)
		
		| RECORD li ->
		(* li is a list of tuple (id, exp) *)
		
			(* inner function : evaluate expressions *)
			let rec eval_list (mem0, li0) =
				(* base case *)
				if (List.length li0) == 0 then (mem0, fun x -> raise (Error ("Not_bound : "^x)))
				
				(* calculate current expression *)
				else let (v, mem1) = eval mem0 env (snd (List.hd li0)) in 
					(* allocate memory to store v *)
					let (l, mem2) = Mem.alloc mem1 in
					(* store v to the new memory location *)
					let mem3 = Mem.store mem2 l v in
					(* record id to bind l (storing value v) *)
					let id = fst (List.hd li0) in
					(* recursively calculate next expressions *)
					let (mem_rec, f_rec) = eval_list(mem3, (List.tl li0)) in
					(* bind new relation id -> l at the f_rec *)
					(mem_rec, fun x -> if x=id then l else f_rec x) in
			
			let (new_mem, record) = eval_list(mem, li) in
			(Record(record), new_mem)
		
		| FIELD (e, x) ->
			(* e : record name, x : field name *)
			let (record, mem') = eval mem env e in
			let r = value_record record in
			let loc = r x in
			let v = Mem.load mem' loc in
			(v, mem')

		(* end of record *)

		(* assign *)
	
    | ASSIGN (x, e) ->
      let (v, mem') = eval mem env e in
      let l = lookup_env_loc env x in
      (v, Mem.store mem' l v)
		
		| ASSIGNF (e1, x, e2) -> 
			(* e1 : record expression, x : record field, e2 : exp. to substitute *)
			let (record, mem1) = eval mem env e1 in
			let (v, mem2) = eval mem1 env e2 in
			let r = value_record record in
			let loc = r x in
			(v, Mem.store mem2 loc v)
		
		(* end of assign *)

    | _ -> failwith "Unimplemented" (* TODO : Implement rest of the cases *)

  let run (mem, env, pgm) = 
    let (v, _ ) = eval mem env pgm in
    v
	
	(* helper functions *)


end
