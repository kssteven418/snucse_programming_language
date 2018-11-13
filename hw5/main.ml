(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)
(*

open Translate
open Pp
open Sm5
open K

let main () =
  let pk = ref false in
  let psm5 = ref false in
  let k = ref false in
  let src = ref "" in
  let filename = Filename.basename Sys.argv.(0) in
  let _ =
    Arg.parse
      [("-pk", Arg.Set pk, "display K- parse tree");
      ("-psm5", Arg.Set psm5, "print translated sm5 code");
      ("-k", Arg.Set k, "run using K interpreter");
      ("-gc", Arg.Set Sm5.gc_mode, "run with garbage collection");
      ("-debug", Arg.Set Sm5.debug_mode, "prints machine state every step")]
      (fun x -> src := x)
      ("Usage: " ^ filename ^ " [-pk | -psm5 | -k] [-gc] [-debug] [file]")
  in
  let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
  let pgm = Parser.program Lexer.start lexbuf in
   
  if !pk then 
    KParseTreePrinter.print pgm
  else if !psm5 then 
    print_endline (Sm5.command_to_str "" (Translator.trans pgm))
  else if !k then 
    ignore (K.run pgm)
  else 
    Sm5.run (Translator.trans pgm)

let _ = main ()
*)

open Sm5.Sm5

let _ = gc_mode := true

let check_exception cmd = 
  try let _ = run cmd in false with GC_Failure -> true

(* concat command n times *)
let append (n: int) (f: int -> command) (cmd: command) : command =
  let rec iter i =
    if i = n then []
    else (f i) @ iter (i + 1) in cmd @ (iter 0)

(* Test 1 : double ftn call, with continuation : FAIL *)
let tcmd1 = 
    let cmds = [

        PUSH (Fn ("t", [
           	MALLOC;
			BIND "x1";
            PUSH (Val (Z 1));
			PUSH (Id "x1");
            STORE;

           	MALLOC;
			BIND "x2";
            PUSH (Val (Z 1));
			PUSH (Id "x2");
            STORE;

           	MALLOC;
			BIND "x3";
            PUSH (Val (Z 1));
			PUSH (Id "x3");
            STORE;

			(* TRIGGER *)
			MALLOC;
        ]));
        BIND "h";

        PUSH (Fn ("t", [
           	MALLOC;
			BIND "x";
            PUSH (Val (Z 1));
			PUSH (Id "x");
            STORE;
				
			PUSH (Id "h");
			PUSH (Val (Z 1));
			MALLOC;
			CALL;
        ]));
        BIND "g";

        PUSH (Fn ("t", [
           	MALLOC;
			BIND "x";
            PUSH (Val (Z 1));
			PUSH (Id "x");
            STORE;
				
			PUSH (Id "g");
			PUSH (Val (Z 1));
			MALLOC;
			CALL;
        ]));
        BIND "f";

		PUSH (Id "f");
		PUSH (Val (Z 1));
		MALLOC;
		CALL;


    ] in
    
    cmds

	
(* Test 2 : double ftn call, without continuation : SUCCESS *)
let tcmd2 = 
    let cmds = [

        MALLOC;
		BIND "x";
        PUSH (Val (Z 100));
		PUSH (Id "x");
        STORE;
        
		PUSH (Fn ("t", [
           	MALLOC;
			BIND "x1";
            PUSH (Val (Z 1));
			PUSH (Id "x1");
            STORE;

           	MALLOC;
			BIND "x2";
            PUSH (Val (Z 1));
			PUSH (Id "x2");
            STORE;

        ]));
        BIND "h";

        PUSH (Fn ("t", [
           	MALLOC;
			BIND "x";
            PUSH (Val (Z 1));
			PUSH (Id "x");
            STORE;
				
			PUSH (Id "h");
			PUSH (Val (Z 1));
			MALLOC;
			CALL;

        ]));
        BIND "g";

        PUSH (Fn ("t", [
           	MALLOC;
			BIND "x";
            PUSH (Val (Z 1));
			PUSH (Id "x");
            STORE;
				
			PUSH (Id "g");
			PUSH (Val (Z 1));
			MALLOC;
			CALL;

        ]));
        BIND "f";

		PUSH (Id "f");
		PUSH (Val (Z 1));
		MALLOC;
		CALL;

		(* TRIGGER *)
		MALLOC;
		BIND "y";
		PUSH (Val (Z 1));
		PUSH (Id "y");
		STORE;

		PUSH (Id "x");
		LOAD;
		PUT;

    ] in
    
    cmds

(* Test 3 : Nested Data Structure : FAIL *)
let tcmd3 = 

  let cmds = [
    MALLOC;
    BIND "x";
    MALLOC;
    BIND "a";
    MALLOC;
    BIND "c";

    MALLOC;
    BIND "d";
    MALLOC;
    BIND "b";
    MALLOC;
    BIND "f";

    MALLOC;
    BIND "e";
    MALLOC;
    BIND "g";

   	(* empty stack top *) 
	PUSH (Id "a");
	PUSH (Id "x");
	STORE;

	PUSH (Id "c");
	PUSH (Id "e");
	STORE;

	PUSH (Id "a");
	PUSH (Id "f");
	STORE;

	PUSH (Id "c");
	PUSH (Id "d");
	STORE;

	PUSH (Id "a");
	PUSH (Id "g");
	STORE;
	
	UNBIND;(*g*)
	UNBIND;(*e*)

	BOX 2;
	PUSH (Id "b");
	STORE;

	UNBIND; (* f *)
	UNBIND; (* b *)
	UNBIND; (* d *)

	BOX 3;
	PUSH (Id "c");
	STORE;

	PUSH (Id "c");
	PUSH (Id "a");
	STORE;

	UNBIND; (* c *)
	UNBIND; (* a *)


	(* TRIGGER *)
	MALLOC
    ]
  in
    
    cmds

(* Test 4 : Nested Data Structure, SUCCESS *)
let tcmd4 = 

  let cmds = [
    MALLOC;
    BIND "x";
    MALLOC;
    BIND "a";
    MALLOC;
    BIND "c";

    MALLOC;
    BIND "d";
    MALLOC;
    BIND "b";
    MALLOC;
    BIND "f";

    MALLOC;
    BIND "e";
    MALLOC;
    BIND "g";

   	(* empty stack top *) 
	PUSH (Id "a");
	PUSH (Id "x");
	STORE;

	PUSH (Id "c");
	PUSH (Id "e");
	STORE;

	PUSH (Id "a");
	PUSH (Id "f");
	STORE;

	PUSH (Id "c");
	PUSH (Id "d");
	STORE;

	PUSH (Id "a");
	PUSH (Id "g");
	STORE;
	
	UNBIND;(*g*)
	UNBIND;(*e*)

	BOX 2;
	PUSH (Id "b");
	STORE;

	UNBIND; (* f *)
	UNBIND; (* b *)
	UNBIND; (* d *)

	BOX 3;
	PUSH (Id "c");
	STORE;

	PUSH (Id "c");
	PUSH (Id "a");
	STORE;

	UNBIND; (* c *)
	UNBIND; (* a *)

	UNBIND; (* x *)

	(* ALL MEMORY DEALLOCATED *)
	MALLOC;
	BIND "1";
	PUSH (Val (Z 1));
	PUSH (Id "1");
	STORE;

	MALLOC;
	BIND "2";
	PUSH (Val (Z 1));
	PUSH (Id "2");
	STORE;

	MALLOC;
	BIND "3";
	PUSH (Val (Z 1));
	PUSH (Id "3");
	STORE;

	MALLOC;
	BIND "4";
	PUSH (Val (Z 1));
	PUSH (Id "4");
	STORE;

	MALLOC;
	BIND "5";
	PUSH (Val (Z 1));
	PUSH (Id "5");
	STORE;

	MALLOC;
	BIND "6";
	PUSH (Val (Z 1));
	PUSH (Id "6");
	STORE;
	MALLOC;

	MALLOC;
	BIND "7";
	PUSH (Val (Z 1));
	PUSH (Id "7");
	STORE;

	MALLOC;
	BIND "8";
	PUSH (Val (Z 1));
	PUSH (Id "8");
	STORE;
 
	(* TRIGGER *)
	(* FAILIRE IF MALLOC; *)

	PUSH(Val (Z 4));
	PUT

    ]
  in
    
    cmds

(* Test 5 : Nested Data Structure, SUCCESS *)
let tcmd5 = 

  let cmds = [
    MALLOC;
    BIND "x";
    MALLOC;
    BIND "a";
    MALLOC;
    BIND "c";

    MALLOC;
    BIND "d";
    MALLOC;
    BIND "b";
    MALLOC;

    BIND "e";
    MALLOC;

    BIND "f";
    MALLOC;
    BIND "g";

   	(* empty stack top *) 
	PUSH (Id "a");
	PUSH (Id "x");
	STORE;

	PUSH (Id "c");
	PUSH (Id "e");
	STORE;

	PUSH (Id "a");
	PUSH (Id "f");
	STORE;

	PUSH (Id "c");
	PUSH (Id "d");
	STORE;

	PUSH (Id "a");
	PUSH (Id "g");
	STORE;
	
	UNBIND;(*g*)
	UNBIND;(*f*)

	BOX 2;
	PUSH (Id "b");
	STORE;

	UNBIND; (* e *)
	POP;
	UNBIND; (* b *)
	UNBIND; (* d *)

	BOX 2;
	PUSH (Id "c");
	STORE;

	PUSH (Id "c");
	PUSH (Id "a");
	STORE;

	UNBIND; (* c *)
	UNBIND; (* a *)


	MALLOC;
	BIND "1";
	PUSH (Val (Z 1));
	PUSH (Id "1");
	STORE;

	(* FAILURE IF MALLOC; *)

	PUSH(Val (Z 5));
	PUT

    ]
  in
    
    cmds

(* 1. Simple malloc & use : trigger gc and success *)
let cmds1 = 
    (* To be collected *)
    let cmds = [
        PUSH (Val (Z 1));
        MALLOC;
        STORE;
    ] in

    let cmds = append 127 (fun i -> 
        let v = Printf.sprintf "x%d" i in [
            MALLOC; 
            BIND v; 
            PUSH (Val (Z 1));
            PUSH (Id v);
            STORE;
        ]) cmds in

    (* Trigger GC *)
    let cmds = cmds @ [
        MALLOC;
        BIND "x_new";
        PUSH (Val (Z 10));
        PUSH (Id "x_new");
        STORE;

        PUSH (Id "x_new");
        LOAD;
    ] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds = append 127 (fun i -> 
        let v = Printf.sprintf "x%d" i in [
            PUSH (Id v);
            LOAD;
            ADD;
         ]) cmds in 

    let cmds = cmds @ [PUT] in

    cmds


(* 2. Simple malloc & use : gc fails *)
let cmds2 = 
    let cmds = append 128 (fun _ -> [
        MALLOC;
        BIND "x"; 
        PUSH (Val (Z 200));
        PUSH (Id "x");
        STORE;
        ]
    ) [] in

    let cmds = cmds @ [
        (* Trigger GC *)
        PUSH (Val (Z 400));
        MALLOC;
        STORE;
        ] in

    (* Access all the allocated memory locations, ensuring they must not have been collected *)
    let cmds = append 128 (fun _ -> [
        PUSH (Id "x");
        LOAD;
        POP;
        
        UNBIND;
        POP;
        ]
    ) cmds in
    cmds

(* 3. Gc must be able to track record : gc fail *)
let cmds3 =
  let cmds = append 126 (fun i -> 
      let v = Printf.sprintf "x%d" i in [
        MALLOC; 
        BIND v; 
        PUSH (Val (Z i));
        PUSH (Id v);
        STORE;
      ])
  [] in

  let cmds = cmds @ [
    MALLOC;
    BIND "loc";
    
    PUSH (Val (Z 1));
    PUSH (Id "loc");
    STORE;

    UNBIND;
    BOX 1
    ]
  in

  let cmds = cmds @ [
    MALLOC;
    BIND "box";

    PUSH (Id "box"); 
    STORE; 

    (* Trigger GC *)
    PUSH (Val (Z 1));
    MALLOC;
    STORE;
  ] in

  (* Access all the allocated memory locations, ensuring they must not have been collected *)
  let cmds = append 126 (fun i -> 
      let v = Printf.sprintf "x%d" i in [
          PUSH (Id v);
          LOAD;
          POP;
       ]) cmds @ [PUSH (Id "box"); LOAD; UNBOX "loc"; LOAD] in

  cmds


(* 4. GC must be able to track locations in the 'Continuation' : gc fails *)
let cmds4 =
    let cmds = [
        PUSH (Fn ("x", [
            (* Trigger GC *)
            PUSH (Val (Z 1));
            MALLOC;
            STORE;

            (* Access argument location, ensuring it must not have been collected *)
            PUSH (Id "x");
            LOAD;
            POP;
        ]));

        BIND "f";
    ] in

    let cmds = append 127 (fun i -> 
        let v = Printf.sprintf "x%d" i in [
            MALLOC;
            BIND v;
            PUSH (Val (Z i));
            PUSH (Id v);
            STORE
        ]) cmds in

    let cmds = cmds @ [
        PUSH (Id "f");
        PUSH (Val (Z 1));
        MALLOC;
        CALL;

    ] in

    (* Access all the allocated memory locations, ensuring they must not have been collected *)
    let cmds = append 127 (fun i -> 
        let v = Printf.sprintf "x%d" i in [
            PUSH (Id v);
            LOAD;
            POP;
         ]) cmds in
    
    cmds


(* Location allocated in function can be collected after return : gc success *)
let cmds5 = 
    let cmds = [
        PUSH (Fn ("x", [
            (* To be collected *)
            MALLOC;
            BIND "local"; 
            PUSH (Val (Z 1));
            PUSH (Id "local");
            STORE;

            (* Access argument location, ensuring it must not have been collected *)
            PUSH (Id "x");
            LOAD;
            POP;
        ]));

        BIND "f";
    ] in

    let cmds = append 126 (fun i -> 
        let v = Printf.sprintf "x%d" i in [
            MALLOC; 
            BIND v; 
            PUSH (Val (Z 5));
            PUSH (Id v);
            STORE;
        ]) cmds in

    let cmds = cmds @ [
        PUSH (Id "f");
        PUSH (Val (Z 1));
        MALLOC;
        CALL;

        (* Trigger GC *)
        PUSH (Val (Z 10));
        MALLOC;
        STORE;
    ] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds = 
      append 126 
        (fun i -> 
          let v = Printf.sprintf "x%d" i in 
            [PUSH (Id v);
            LOAD;
            ADD]
        ) (cmds @ [PUSH (Val (Z 0));]) in 

    let cmds = cmds @ [PUT] in
    cmds

(* GC must not miss a location with different offset *)
let cmds6 = 
  (* Location to be collected *)
  let cmds = [PUSH (Val (Z 1)); MALLOC; STORE] in
  
  (* Allocate, bind and store 126 times *)
  let cmds = append 126 (fun i -> 
    let v = Printf.sprintf "x%d" i in [
        MALLOC; 
        BIND v; 
        PUSH (Val (Z 1));
        PUSH (Id v);
        STORE;
    ]) cmds 
  in
  
  (* Another location with same base, different offset *)
  let cmds = cmds @ 
    [PUSH (Val (Z 500)); 
    PUSH (Id "x0"); 
    PUSH (Val (Z 10)); 
    ADD; 
    STORE;   (* Env : "x0" ==> (a, 0) / Mem : (a, 10) ==> 500 *)
    ]
  in
  
  (* Trigger GC *)
  let cmds = cmds @ [
    MALLOC;
    BIND "foo";
    PUSH (Val (Z 1));
    PUSH (Id "foo");
    STORE
    ]
  in

  cmds @ [PUSH (Id "x0"); PUSH (Val (Z 10)); ADD; LOAD; PUT]

(*
let _ = run cmds1 (* 137 *)
let _ = print_endline (string_of_bool (check_exception cmds2)) (* true *)
let _ = print_endline (string_of_bool (check_exception cmds3)) (* true *)
let _ = print_endline (string_of_bool (check_exception cmds4)) (* true *)
let _ = run cmds5 (* 630 *)
let _ = run cmds6 (* 500 *)
*)

let _ = print_endline (string_of_bool (check_exception tcmd1)) (* true *)
let _ = run tcmd2 (* 100 *)
let _ = print_endline (string_of_bool (check_exception tcmd3)) (* true *)
let _ = run tcmd4 (* 22 *)
let _ = run tcmd5 (* 22 *)
