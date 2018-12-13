(*
 * SNU 4190.310 Programming Languages
 * Type Checker Skeleton
 *)


open M

type var = string

type typ = 
  | TInt
  | TBool
  | TString
  | TPair of typ * typ
  | TLoc of typ
  | TFun of typ * typ
  | TVar of var
	| TIBS of var (* should be either int, bool or string *)
	| TIBSL of var (* should be either int, bool, string or location *)
  (* Modify, or add more if needed *)

type typ_scheme =
  | SimpleTyp of typ 
  | GenTyp of (var list * typ)

type typ_env = (M.id * typ_scheme) list

let count = ref 0 

let new_var () = 
  let _ = count := !count +1 in
  "x_" ^ (string_of_int !count)

(* Definitions related to free type variable *)

let union_ftv ftv_1 ftv_2 = 
  let ftv_1' = List.filter (fun v -> not (List.mem v ftv_2)) ftv_1 in
  ftv_1' @ ftv_2
  
let sub_ftv ftv_1 ftv_2 =
  List.filter (fun v -> not (List.mem v ftv_2)) ftv_1

let rec ftv_of_typ : typ -> var list = function
  | TInt | TBool | TString -> []
  | TPair (t1, t2) -> union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TLoc t -> ftv_of_typ t
  | TFun (t1, t2) ->  union_ftv (ftv_of_typ t1) (ftv_of_typ t2)
  | TVar v -> [v]
	| TIBS v -> [v]
	| TIBSL v -> [v]

let ftv_of_scheme : typ_scheme -> var list = function
  | SimpleTyp t -> ftv_of_typ t
  | GenTyp (alphas, t) -> sub_ftv (ftv_of_typ t) alphas 

let ftv_of_env : typ_env -> var list = fun tyenv ->
  List.fold_left 
    (fun acc_ftv (id, tyscm) -> union_ftv acc_ftv (ftv_of_scheme tyscm))
    [] tyenv 

(* Generalize given type into a type scheme *)
let generalize : typ_env -> typ -> typ_scheme = fun tyenv t ->
  let env_ftv = ftv_of_env tyenv in
  let typ_ftv = ftv_of_typ t in
  let ftv = sub_ftv typ_ftv env_ftv in
  if List.length ftv = 0 then
    SimpleTyp t
  else
    GenTyp(ftv, t)

(* Definitions related to substitution *)

type subst = typ -> typ

let empty_subst : subst = fun t -> t

let make_subst : var -> typ -> subst = fun x t ->
  let rec subs t' = 
    match t' with
    | TVar x' 
		| TIBS x'
		| TIBSL x' -> if (x = x') then t else t'
    | TPair (t1, t2) -> TPair (subs t1, subs t2)
    | TLoc t'' -> TLoc (subs t'')
    | TFun (t1, t2) -> TFun (subs t1, subs t2)
    | TInt | TBool | TString -> t'
  in subs

let (@@) s1 s2 = (fun t -> s1 (s2 t)) (* substitution composition *)

let subst_scheme : subst -> typ_scheme -> typ_scheme = fun subs tyscm ->
  match tyscm with
  | SimpleTyp t -> SimpleTyp (subs t)
  | GenTyp (alphas, t) ->
    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> make_subst alpha (TVar beta) @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

(* subst_env S Gamma == SGamma *)
let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv




(* TODO : Implement this function *)
	
let search_env : typ_env -> M.id -> typ_scheme = fun gamma x ->
	
	(* gamma : id, scheme list *)
	(* find elmt that has same id *)
	let pred = fun (i', ts') -> i' = x in 
	let (i, ts) = List.find pred gamma in
	ts

let rec var_in_type = fun t a ->
	match t with 
	| TPair(t1, t2) 
	| TFun(t1, t2) -> (var_in_type t1 a) || (var_in_type t2 a)
	| TLoc t' -> var_in_type t' a
	| TVar t' -> t'=a
	| TIBS t' -> t'=a
	| TIBSL t' -> t'=a
	| _ -> false
	
let prt : typ -> unit =
  let rec iter : typ -> unit =
	    fun t ->
			      match t with
						      | TInt -> print_string "Int"
									      | TBool -> print_string "Bool"
												      | TString -> print_string "String"
															      | TPair (t1, t2) ->
																		          let _ = print_string "(" in
																							          let _ = iter t1 in
																												          let _ = print_string ", " in
																																	          let _ = iter t2 in
																																						          print_string ")"
																																											      | TFun (t1, t2) ->
																																														          let _ = print_string "(" in
																																																			          let _ = iter t1 in
																																																								          let _ = print_string " → " in
																																																													          let _ = iter t2 in
																																																																		          print_string ")"
																																																																							      | TLoc t ->
																																																																										          let _ = iter t in
																																																																															          print_string "_Loc"
																																																																																				      | TVar v -> print_string ("Var " ^ v)
																																																																																							      | TIBS v -> print_string ("IBS " ^ v)
																																																																																										      | TIBSL v -> print_string ("IBSL " ^ v)
																																																																																													    in
																																																																																															  fun t ->
																																																																																																      let _ = iter t in
																																																																																																			      print_string "\n"


let rec unify : typ * typ -> subst = fun (t1, t2) ->
	(* exactly matching *)
	if t1=t2 then empty_subst
	else match(t1, t2) with
	
	(* pairwise types *)
	| (TFun(x, y), TFun(x', y')) ->
		let s = unify(x, x') in
		let s' = unify(s y, s y') in
		(s @@ s')
	| (TPair(x, y), TPair(x', y')) ->
		let s = unify(x, x') in
		let s' = unify(s y, s y') in
		(s @@ s')
	| (TLoc x, TLoc y) ->
		let s = unify(x, y) in
		s
	
	(* type ans variable -> variable must not be in the type *)
	| (t, TVar a)
	| (TVar a, t) ->
		if (var_in_type t a) then (* fails *)
			raise (M.TypeError "unify failure TVar")
		else 
		(match t with
			| _ -> 	make_subst a t
		)

	(* IBSL type *)
	| (t, TIBSL a)
	| (TIBSL a, t) ->
		if (var_in_type t a) then (* fails *)
			raise (M.TypeError "unify failure TIBSL, var not in")
		else
		(* must be one of int, bool, string, or loc type *)
		(match t with 
			| TInt
			| TBool
			| TString
			| TLoc _
			| TIBS _ (* TIBS is subset of TIBSL *)
			| TIBSL _ (* or itself *) 
				-> make_subst a t
			| _ -> 
			raise (M.TypeError "unify failure TIBSL")
		)

	(* IBS type *)
	| (t, TIBS a)
	| (TIBS a, t) ->
		if (var_in_type t a) then (* fails *)
			raise (M.TypeError "unify failure TIBS, val not in")
		else
		(* must be one of int, bool, or string type *)
		(match t with 
			| TInt
			| TBool
			| TString 
			| TIBS _ (* or itself *) 
				-> make_subst a t
			| _ -> 
			raise (M.TypeError "unify failure TIBS")
		)

	| _ -> 
		let _ = prt t1 in
        let _ = prt t2 in 
		raise (M.TypeError "unify failure, no...") 

let rec expansive = fun exp ->
	match exp with
	| M.CONST c -> false
	| M.VAR v -> false
	| M.FN (x, e) -> false
	| M.READ -> false
	| M.WRITE e -> false
	| M.BANG e -> false
	| M.APP (e1, e2) -> true
	| M.MALLOC e -> true
 	| M.ASSIGN (e1, e2) -> (expansive e1)||(expansive e2)
	| M.IF (e1, e2, e3) -> (expansive e1)||(expansive e2)||(expansive e3)
	| M.LET (M.VAL (x, e1), e2) -> (expansive e1)||(expansive e2)
	| M.LET (M.REC (f, x, e1), e2) -> (expansive e1)||(expansive e2)
	| M.BOP (op, e1, e2) -> (expansive e1)||(expansive e2)
	| M.SEQ (e1, e2) -> (expansive e1)||(expansive e2)
	| M.PAIR (e1, e2) -> (expansive e1)||(expansive e2) 
	| M.FST e -> expansive e
	| M.SND e -> expansive e

let just = (empty_subst, TInt) (* just for debugging *)

let rec w : typ_env * M.exp -> subst * typ = fun (gamma, exp) ->
	match exp with
	| M.CONST (M.S s) -> (empty_subst, TString)
	| M.CONST (M.N n) -> (empty_subst, TInt)
	| M.CONST (M.B b) -> (empty_subst, TBool)

	| M.VAR v -> 
		begin 
		try
			let ts = search_env gamma v in
			(* this function with empty_subst will automatically build {ai->bi}t *)
			let new_ts = subst_scheme empty_subst ts in
			(match new_ts with
				| SimpleTyp t -> (empty_subst, t)
				| GenTyp (_, t) -> (empty_subst, t)
			)
		with Not_found -> raise (M.TypeError (v^" is not declared"))
		end

	| M.FN (x, e) -> 
		let beta = TVar(new_var()) in
		(* add x:beta in the env. *)
		let gamma' = [(x, SimpleTyp(beta))]@gamma in 
		let (s, t) = w(gamma', e) in
		(s, s(TFun(s beta, t)))


	| M.APP (e1, e2) -> 
		let (s1, t1) = w(gamma, e1) in
		let (s2, t2) = w(subst_env s1 gamma, e2) in
		let beta = TVar(new_var()) in
		(* first statement : ftn type *)
		let s3 = unify(s2 t1, TFun(t2, beta)) in
		let s' = (s3 @@ s2 @@ s1) in
		(s', s' beta)

	| M.LET (d, e2) -> 
		( match d with 
			| M.REC (f, x, e1) -> 
				(* bind the function name f *)
				let beta = TVar(new_var()) in
				let gamma' = [(f, SimpleTyp beta)] @ gamma in
				(* with f-binded environment gamma' *)
				(* check the function x=>e1 *)
				let (s1, t1) = w(gamma', M.FN(x, e1)) in
				(* beta : type of f -> should be same as t *)
				let s_uni = unify(s1 beta, t1) in
				let s' = s_uni @@ s1 in
				(* generalize!! *)
				let gamma'' = 
					if(expansive (M.FN(x, e1))) then [(f, SimpleTyp (s' t1))] @ gamma'
					else [(f, generalize gamma' (s' t1))] @ gamma' in
				let (s2, t2) = w(gamma'', e2) in
				let s' = s2 @@ s_uni @@ s1 in
				(s', s' t2)

			| M.VAL (x, e1) ->
				let (s1, t1) = w(gamma, e1) in
				let gamma' = subst_env s1 gamma in
				(* generalize!! *)
				let gamma'' =
					if (expansive e2) then [(x, SimpleTyp t1)] @ gamma'
					else [(x, generalize gamma' t1)] @ gamma' in
				let (s2, t2) = w(gamma'', e2) in
				let s' = (s2 @@ s1) in
				(s', s' t2)
		)

	| M.BOP (op, e1, e2) -> 
		(match op with
		 	| M.ADD
		 	| M.SUB ->
		 		let (s1, t1) = w(gamma, e1) in
				let s_uni1 = unify(t1, TInt) in
				let s' = s_uni1 @@ s1 in
				let gamma' = subst_env s' gamma in
				let (s2, t2) = w(gamma', e2) in
				let s_uni2 = unify(t2, TInt) in
				let s' = s_uni2 @@ s2 @@ s_uni1 @@ s1 in
				(s', TInt)

			| M.AND
			| M.OR ->
		 		let (s1, t1) = w(gamma, e1) in
				let s_uni1 = unify(t1, TBool) in
				let s' = s_uni1 @@ s1 in
				let gamma' = subst_env s' gamma in
				let (s2, t2) = w(gamma', e2) in
				let s_uni2 = unify(t2, TBool) in
				let s' = s_uni2 @@ s2 @@ s_uni1 @@ s1 in
				(s', TBool)
						
			| M.EQ ->
		 		let (s1, t1) = w(gamma, e1) in
				let s_uni1 = unify(t1, TIBSL(new_var())) in
				let s' = s_uni1 @@ s1 in
				let gamma' = subst_env s' gamma in
				let (s2, t2) = w(gamma', e2) in
				let s_uni2 = unify(t2, TIBSL(new_var())) in
				let s_uni3 = unify(t1, t2) in
				let s' = s_uni3 @@ s_uni2 @@ s2 @@ s_uni1 @@ s1 in
				(s', TBool)
		)


	| M.IF (e1, e2, e3) ->
		(* condition : bool *)
		let (s1, t1) = w(gamma, e1) in
		let s_cond = unify(t1, TBool) in

		(* if and else statement : should have same type *)
		let s' = s_cond @@ s1 in
		let gamma' = subst_env s' gamma in
		let (s2, t2) = w(gamma', e2) in
		let s' = (s2 @@ s_cond @@ s1) in
		let gamma' = subst_env s' gamma in
		let (s3, t3) = w(gamma', e3) in
		let s_stmt = unify(t2, t3) in
		let s' = s_stmt @@ s3 @@ s2 @@ s_cond @@ s1 in
		(s', s' t3)
		
	| M.READ -> (empty_subst, TInt)

	| M.WRITE e -> 
		let (s, t) = w(gamma, e) in
		let v_ibs = TIBS(new_var()) in
		let s_ibs = unify(t, v_ibs) in
		let s' = s_ibs @@ s in
		(s', s' t)

	| M.MALLOC e -> (* malloc e *)
		let (s, t) = w(gamma, e) in
		(s, s (TLoc t))

	| M.ASSIGN (e1, e2) -> (* e1 := e2 *)
		let (s1, t1) = w(gamma, e1) in
		let gamma' = subst_env s1 gamma in
		let (s2, t2) = w(gamma', e2) in
		(* first elmt should be a location type *)
		(match t1 with
		 	| TLoc l ->
				(* loc type and the second type should be same *)
				let s_loc = unify(l, t2) in
				let s' = s_loc @@ s2 @@ s1 in
				(s', s' t2) 
			| TVar v -> 
				let l = TVar(new_var()) in
				let s' = unify(TLoc l, t1) in
				let s'' = unify(l, t2) in
				let snew = s''@@s'@@s2@@s1 in
				(snew, snew t2)
				
			| _ -> raise (M.TypeError "cannot be assigned")
		)
	
	| M.BANG e -> (* !e *)
		(* e must have a location type *)
		let (s, t) = w(gamma, e) in
		(match t with 
			| TLoc l -> (s, s l)
			| TVar v ->
				let beta = TVar(new_var()) in
				let snew = make_subst v (TLoc beta) in
				let s' = snew @@ s in
				(s', s' beta)
			| _-> raise (M.TypeError "error at BANG")
		)
				 
	| M.SEQ (e1, e2) -> (* e1; e2 *)
		let (s1, t1) = w(gamma, e1) in
		let gamma' = subst_env s1 gamma in
		let (s2, t2) = w(gamma', e2) in
		let s' = s2 @@ s1 in
		(s', s' t2)
	
	| M.PAIR (e1, e2) -> (* (e1, e2) *)
		let (s1, t1) = w(gamma, e1) in
		let gamma' = subst_env s1 gamma in
		let (s2, t2) = w(gamma', e2) in
		let s' = s2 @@ s1 in
		(s', s'(TPair (t1, t2)))
	
	| M.FST e ->
		(* e must be a pair type *)
		let	(s, t) = w(gamma, e) in
		(match t with
			| TPair (t1, t2) -> (s, s t1)
			| TVar v ->
				let beta1 = TVar(new_var()) in
				let beta2 = TVar(new_var()) in
				let snew = make_subst v (TPair(beta1, beta2)) in
				let s' = snew @@ s in
				(s', s' beta1)
			| _ -> raise (M.TypeError "error at FST") 
		)

	| M.SND e ->
		(* e must be a pair type *)
		let	(s, t) = w(gamma, e) in
		(match t with
			| TPair (t1, t2) -> (s, s t2)
			| TVar v ->
				let beta1 = TVar(new_var()) in
				let beta2 = TVar(new_var()) in
				let snew = make_subst v (TPair(beta1, beta2)) in
				let s' = snew @@ s in
				(s', s' beta2)
			| _ -> raise (M.TypeError "error at SND") 
		)
		(*
		 | M.FST e ->
			(* e must be a pair type *)
			let	(s, t) = w(gamma, e) in
			let beta1 = TVar(new_var()) in
			let beta2 = TVar(new_var()) in
			let s_uni = unify(t, TPair(beta1, beta2)) in
			let s' = s_uni @@ s in
			(s', s' beta1)
	 
	 	| M.SND e ->
			(* e must be a pair type *)
			let	(s, t) = w(gamma, e) in
			let beta1 = TVar(new_var()) in
			let beta2 = TVar(new_var()) in
			let s_uni = unify(t, TPair(beta1, beta2)) in
			let s' = s_uni @@ s in
			(s', s' beta2)
		*)

let rec output_type = fun t ->
	match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (output_type t1, output_type t2)
  | TLoc l -> M.TyLoc (output_type l)
	| _ -> 
	raise(M.TypeError "Type is not determined in closed form")

let check : M.exp -> M.typ = fun exp ->
  let (_, t) = w ([], exp) in
	output_type t
