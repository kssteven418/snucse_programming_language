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





let tchecklist_e = ref [] 
let tchecklist_w = ref [] 

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

	let _ = print_endline "At generalization" in
	let rec iter l = 
		if (List.length l)=0 then ()
		else let _ = print_endline (List.hd l) in
			iter (List.tl l) in
	let _ = iter ftv in
	let _ = print_endline "" in
	

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

		let _ = print_endline "At subst_scheme" in
		let _ = prt t in
		let _ = print_endline "" in

    (* S (\all a.t) = \all b.S{a->b}t  (where b is new variable) *)
    let betas = List.map (fun _ -> new_var()) alphas in
    let s' =
      List.fold_left2
        (fun acc_subst alpha beta -> 
				 let temp = 
				 		if List.mem alpha !tchecklist_e then
							let _ = tchecklist_e := (!tchecklist_e)@[beta] in
							TIBSL beta
						else if List.mem alpha !tchecklist_w then 
							let _ = tchecklist_w := (!tchecklist_w)@[beta] in
							TIBS beta
						else TVar beta in
				 make_subst alpha temp @@ acc_subst)
        empty_subst alphas betas
    in
    GenTyp (betas, subs (s' t))

(* subst_env S Gamma == SGamma *)
let subst_env : subst -> typ_env -> typ_env = fun subs tyenv ->
  List.map (fun (x, tyscm) -> (x, subst_scheme subs tyscm)) tyenv




(* TODO : Implement this function *)
	
let rec var_in_type = fun t a ->
	match t with 
	| TPair(t1, t2) 
	| TFun(t1, t2) -> (var_in_type t1 a) || (var_in_type t2 a)
	| TLoc t' -> var_in_type t' a
	| TVar t' -> t'=a
	| TIBS t' -> t'=a
	| TIBSL t' -> t'=a
	| _ -> false
	
																																																																																																					

let rec unify : typ * typ -> subst = fun (t1, t2) ->
	match(t1, t2) with
	(* exactly matching *)
  | (TInt, TInt) -> empty_subst
  | (TBool, TBool) -> empty_subst
  | (TString, TString) -> empty_subst
	
	(* type ans variable -> variable must not be in the type *)
	| (t, TVar a)
	| (TVar a, t) ->
		if t1=t2 then empty_subst
		else if (var_in_type t a) then (* fails *)
			raise (M.TypeError "unify failure TVar")
		else 
		(match t with
			| _ -> 	make_subst a t
		)

	(* IBSL type *)
	| (t, TIBSL a)
	| (TIBSL a, t) ->
		if t1=t2 then empty_subst
		else if (var_in_type t a) then (* fails *)
			let _ = prt t in
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
		if t1=t2 then empty_subst
		else if (var_in_type t a) then (* fails *)
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
	(* pairwise types *)
	| (TPair(x, y), TPair(x', y')) ->
		let s = unify(x, x') in
		let s' = unify(s y, s y') in
		(s' @@ s)
	| (TLoc x, TLoc y) ->
		let s = unify(x, y) in
		s
	| (TFun(x, y), TFun(x', y')) ->
		let s = unify(x, x') in
		let s' = unify(s y, s y') in
		(s' @@ s)

	| _ -> 
	(*
		let _ = prt t1 in
        let _ = prt t2 in 
				*)
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

let deb = fun msg v ->
	let temp = msg ^ " " ^ v in
	let _ = print_endline temp in 
	let _ = print_endline " " in
	()

let search_env : typ_env -> M.id -> typ_scheme = fun gamma x ->
	
	(* gamma : id, scheme list *)
	(* find elmt that has same id *)
	let pred = fun (i', ts') -> i' = x in 
	let (i, ts) = List.find pred gamma in
	ts

let rec m : typ_env * M.exp * typ -> subst = fun (gamma, exp, t) ->
	match exp with
	| M.CONST (M.S s) -> unify(t, TString)
	| M.CONST (M.N n) -> unify(t, TInt)
	| M.CONST (M.B b) -> unify(t, TBool)

	| M.VAR v -> 
		let _ = print_endline ("typr of var "^v) in
		let _ = prt t in
		(*
				let get_type_scheme : typ_env -> M.id -> typ_scheme =
    			fun env i ->
        		if (List.exists (fun a -> (fst a) = i) env)
        			then (snd (List.find (fun a -> (fst a) = i) env))
        		else
            	raise (M.TypeError "ID doesn't exist in Env") in
							
				let rec instantiate' vl t =
    			match vl with
    			| [] -> t
    			| hd::tl ->
            let v = new_var() in
            let new_type = make_subst hd (TVar v) in
            (instantiate' tl new_type) @@ t in

				let rec instantiate ts =
    			match ts with
    			| SimpleTyp t1 -> t1
    			| GenTyp (vl, t1) ->
            let s = instantiate' vl empty_subst in
            s t1 in

				let ts = get_type_scheme gamma v in
				let its = instantiate ts in
                unify (t, its)
		*)
		begin 
		try
			let ts = search_env gamma v in
			
			(*
			(* start of build_type function *)
			let build_type ts = 
				
				let rec iter vl = 
					if((List.length ts)=0) then empty_subst
					else 
						let add_subst = make_subst (List.hd vl) (TVar (new_var ())) in
						(iter (List.tl vl)) @@ add_subst in
				
				match ts with
				| GenTyp (vl, t') -> 
					let s = iter vl in
					(s t') 
				| SimpleTyp t' -> t' in
			(* end of build_type function *)
			
			let new_t = build_type ts in
			(* this function with empty_subst will automatically build {ai->bi}t *)i 	
			*)
			let new_ts =  subst_scheme empty_subst ts in
			(match new_ts with
				| SimpleTyp t' ->
					let _ = print_endline "simple" in  unify(t, t')
				| GenTyp (_, t') ->
					let _ = print_endline "general" in
					let _ = prt t' in
					unify(t, t')
			)
			with Not_found -> raise (M.TypeError (v^" is not declared!"))
		end

	| M.FN (x, e) -> 
		let b1 = new_var() in
		let b2 = new_var() in
		let _ = deb "FN" b1 in 
		let _ = deb "FN" b2 in 
		let s1 = unify(t, TFun (TVar b1, TVar b2)) in
		let gamma' = [(x, SimpleTyp (s1 (TVar b1)))] @ (subst_env s1 gamma) in
		let s2 = m(gamma', e, (s1 (TVar b2))) in
		s2 @@ s1

	| M.APP (e1, e2) -> 
		let b = new_var() in
		let _ = deb "APP" b in 
		let s1 = m(gamma, e1, TFun (TVar b, t)) in
		let gamma' = subst_env s1 gamma in
		let s2 = m(gamma', e2, s1 (TVar b)) in
		s2 @@ s1


	| M.LET (d, e2) -> 
		( match d with 
			| M.REC (f, x, e1) ->
				let b1 = new_var() in
				let b2 = new_var() in

				let new_env = 
					if(expansive e1) then [(f, SimpleTyp(TFun(TVar b1, TVar b2)))]
					else [(f, generalize gamma (TFun(TVar b1, TVar b2)))] in
				let gamma1 = new_env @ gamma in
				let s1 = m(gamma1, M.FN(x, e1), TFun(TVar b1, TVar b2)) in
				
				let gamma2 = subst_env s1 gamma in
				let new_env2 
					= [(f, generalize gamma2 (s1 (TFun(TVar b1, TVar b2))))] in
				let gamma3 = new_env2 @ gamma2 in

				let s2 = m(gamma3, e2, (s1 t)) in
				s2 @@ s1


			| M.VAL (x, e1) ->
				let b = new_var() in
				let _ = deb "LETVAL" b in 
				let s1 = m(gamma, e1, TVar b) in
				let gamma' = subst_env s1 gamma in (* S1 Gamma *)
				let new_env = 
					if(expansive e1) then [(x, SimpleTyp (s1(TVar b)))]
					else [(x, generalize gamma' (s1(TVar b)))] in
				let gamma'' = new_env @  gamma' in
				let s2 = m(gamma'', e2, s1 t) in
				s2 @@ s1
		)

	| M.BOP (op, e1, e2) -> 
		(match op with
		 	| M.ADD
		 	| M.SUB ->
				let s1 = unify (t, TInt) in
				let gamma' = subst_env s1 gamma in
				let s2 = m(gamma', e1, s1 TInt) in
				let gamma'' = subst_env s2 gamma' in
				let s2' = s2 @@ s1 in
				let s3 = m(gamma'', e2, s2' TInt) in
				let s3' = s3 @@ s2 @@ s1 in
				s3'
			| M.AND
			| M.OR ->
				let s1 = unify (t, TBool) in
				let gamma' = subst_env s1 gamma in
				let s2 = m(gamma', e1, s1 TBool) in
				let gamma'' = subst_env s2 gamma' in
				let s2' = s2 @@ s1 in
				let s3 = m(gamma'', e2, s2' TBool) in
				let s3' = s3 @@ s2 @@ s1 in
				s3'
			| M.EQ ->
				let b = new_var() in
				let s1 = unify (t, TBool) in
				let gamma' = subst_env s1 gamma in
				let s2 = m(gamma', e1, s1 (TIBSL b)) in
				let gamma'' = subst_env s2 gamma' in 
				let s2' = s2 @@ s1 in
				let s3 = m(gamma'', e2, s2' (TIBSL b)) in
				let s3' = s3 @@ s2 @@ s1 in
				let _ = tchecklist_e := (!tchecklist_e)@[b] in
				s3'
		)

		| M.READ -> unify(t, TInt) 
		
		(* e2 must be t type *)
		| M.SEQ (e1, e2) ->
			let b = new_var() in
			let s1 = m(gamma, e1, (TVar b)) in
			let gamma' = subst_env s1 gamma in
			let s2 = m(gamma', e2, (s1 t)) in
			s2 @@ s1

		| M.PAIR (e1, e2) ->
			(*
			let _ = print_endline "PAIR" in
			let _ = prt t in
			let _ = print_endline "" in
			*)
			(* t must be a pair (b1, b2) type *)
			let b1 = new_var() in
			let b2 = new_var() in
			let s1 = unify(t, TPair(TVar b1, TVar b2)) in
			let gamma' = subst_env s1 gamma in
			(* and b1 and b2 must be types of e1 and e2 resp. *)
			let s2 = m(gamma', e1, s1(TVar b1)) in
			let gamma'' = subst_env s2 gamma' in
			let s2' = s2 @@ s1 in
			let s3 = m(gamma'', e2, s2'(TVar b2)) in
			let s3' = s3 @@ s2 @@ s1 in 
			s3'
			
		| M.FST e ->
			(*
			let _ = print_endline "FST" in
			let _ = prt t in
			let _ = print_endline "" in
			*)
			let b = new_var() in
			let s = m(gamma, e, TPair(t, TVar b)) in
			s

		| M.SND e ->
			(*
			let _ = print_endline "SND" in
			let _ = prt t in
			let _ = print_endline "" in
			*)
			let b = new_var() in
			let s = m(gamma, e, TPair(TVar b, t)) in
			s

		| M.MALLOC e ->
			let b = new_var() in
			let s1 = unify (t, (TLoc(TVar b))) in
			let gamma' = subst_env s1 gamma in
			let s2 = m(gamma', e, (s1 (TVar b))) in
			s2 @@ s1

	 	| M.ASSIGN (e1, e2) ->
			(* e1 := e2 *)
			(* e2 must be t type and e1 must be loc t type *)
			let s1 = m(gamma, e2, t) in
			let gamma' = subst_env s1 gamma in
			let s2 = m(gamma', e1, (s1(TLoc t))) in
			s2 @@ s1

		| M.BANG e -> 
			let s = m(gamma, e, (TLoc t)) in
			s

		| M.IF (e1, e2, e3) ->
			(* e1 must be bool type *)
			let s1 = m(gamma, e1, TBool) in
			let gamma' = subst_env s1 gamma in
			let s2 = m(gamma', e2, s1 t) in
			let gamma'' = subst_env s2 gamma' in
			let s2' = s2 @@ s1 in
			let s3 = m(gamma'', e3, s2' t) in
			let s3' = s3 @@ s2 @@ s1 in
			s3'

		| M.WRITE e ->
			let b = new_var() in
			let s1 = unify(t, (TIBS b)) in
			(*
			let _ = print_endline "write" in
			let _ = prt t in
			let s1 = unify(t, (TVar b)) in
			*)
			let _ = tchecklist_w := (!tchecklist_w)@[b] in
			let gamma' = subst_env s1 gamma in
			let s2 = m(gamma', e, s1 t) in
			s2 @@ s1


(* debugging *)
let prt_list = fun l ->
	let rec iter = fun l ->
		if (List.length l) = 0 then ""
		else 
			let (var, c) = List.hd l in
			let str = "(" ^ var ^ ", " ^ (string_of_int c) ^ ") " in
			str ^ (iter (List.tl l)) in
	let _ = print_endline (iter l) in
	()
	
			

let rec output_type = fun t ->
	match t with
  | TInt -> M.TyInt
  | TBool -> M.TyBool
  | TString -> M.TyString
  | TPair (t1, t2) -> M.TyPair (output_type t1, output_type t2)
  | TLoc l -> M.TyLoc (output_type l)
	| _ -> 
	let _ = prt t in
	raise(M.TypeError "Type is not determined in closed form")

let check : M.exp -> M.typ = fun exp ->
(*
  let (_, t) = w ([], exp) in
	output_type t
	*)

	let init_val = new_var() in
	let s = m([], exp, TVar(init_val)) in
	let result = s (TVar(init_val)) in
	(*let _ = prt_list(!tchecklist) in *)

	let _ = print_endline "" in
	let _ = print_endline "x_1" in
	let t = s (TVar("x_1")) in
	let _ = prt t in

	let _ = print_endline "x_2" in
	let t = s (TVar("x_2")) in
	let _ = prt t in

	let _ = print_endline "x_3" in
	let t = s (TVar("x_3")) in
	let _ = prt t in

	let _ = print_endline "x_4" in
	let t = s (TVar("x_4")) in
	let _ = prt t in

	let _ = print_endline "x_5" in
	let t = s (TVar("x_5")) in
	let _ = prt t in

	let _ = print_endline "x_6" in
	let t = s (TVar("x_6")) in
	let _ = prt t in

	let _ = print_endline "x_7" in
	let t = s (TVar("x_7")) in
	let _ = prt t in

	let _ = print_endline "x_8" in
	let t = s (TVar("x_8")) in
	let _ = prt t in

	let _ = print_endline "x_9" in
	let t = s (TVar("x_9")) in
	let _ = prt t in

	let _ = print_endline "x_10" in
	let t = s (TVar("x_10")) in
	let _ = prt t in

	let _ = print_endline "x_11" in
	let t = s (TVar("x_11")) in
	let _ = prt t in

	let _ = print_endline "x_12" in
	let t = s (TVar("x_12")) in
	let _ = prt t in

	let _ = print_endline "" in


	output_type result


