open Hashtbl;;
open List;;

type func = {
	mutable code: string;
	mutable comments: string;
	mutable vars: int;
	mutable symtbl: (string, int) Hashtbl.t; (* symbol table *)
	mutable params: string list; (* parameter list *)
	mutable labeltbl: int; (* label counter *)
	mutable isPrototype: bool;
	mutable header: string;
};;

(* return new empty function *)
let new_func () = {
	code = "";
	comments = "";
	vars = 0;
	symtbl = Hashtbl.create 20;
	params = [];
	labeltbl = 0;
	isPrototype = false;
	header = "";
};;

let set_header fn str = fn.header <- str;;

let set_prototype fn = fn.isPrototype <- true; fn;;

(* adds a new local var declaration to f *)
let new_named_var (f: func) (id: string) = let i = f.vars in
		ignore(f.vars <- f.vars + 1);
		ignore(f.symtbl <- (let x = (add f.symtbl id i); f.symtbl in x));
		f
;;

(* returns position in local array of newly allocated variable. TODO: MUST MODIFY TO: mem[fp + f.vars] *)
let new_local_var (f: func) = let i = string_of_int f.vars in ignore(f.vars <- f.vars + 1); "mem[fp + " ^ i ^ "]";;

(* add new parameter to f, in parameter list and symbol table *)
let add_param (f: func) (id: string) = 
	let i = f.vars in 
	ignore(f.vars <- f.vars + 1);
	ignore(f.symtbl <- (let x = (add f.symtbl id i); f.symtbl in x));
	ignore(f.params <- (match f.params with | [] -> [id] | _ -> id::f.params));
	f
;;

let append (f:func) (str:string) = ignore(f.code <- f.code ^ str); f;; (* add code to f *)

let add_comment (f:func) (str:string) = ignore(f.comments <- f.comments ^ str);; 

let new_label (f:func) = ignore(f.labeltbl <- f.labeltbl + 1); "L" ^ (string_of_int f.labeltbl);; (* returns a new label, for cond statements *)
let prev_label_num (f:func) = string_of_int f.labeltbl;;

let is_param (f:func) id = List.mem id f.params;; (* check if id is parameter of f *)

let include_id (f:func) id = Hashtbl.mem f.symtbl id;; (* check if id is in symbol table *)

let get_code f = f.code;;

let get_comments f = f.comments;;

let num_params f = List.length f.params;;

let id_to_num f id = Hashtbl.find f.symtbl id;;
(* Requires 4.05 or later
let id_to_num (f:func) id = match (find_opt f.symtbl id) with | None -> raise (Failure "undeclared identifier") | Some s -> string_of_int s;; (* find location of id in local array *)
*)

(* Generate code to push parameters onto stack before goto. TODO: remove fn from pushparams calls *)
let pushparams l = 
	let rec helper l str i = let j = string_of_int i in 
								match l with 	| [] -> str
												| h::tl -> helper tl (str ^ "mem[sp + " ^ j ^ "] = " ^ h ^ ";\n") (i+1)
    in helper l "" 0;;

(*
 Generates code which sets mem[fp + 0, fp+ + 1, ..., fp + (k-1)] to parameters pushed onto stack by caller at mem[fp - 3 - k, fp - 3 - k + 1, ... , fp - 4] respectively
*)
let paramset len = 
	let rec helper len str i = let j = string_of_int i and k = string_of_int (3 + (len - i)) in
								if (i >= len) then str
								else helper len (str ^ "mem[fp + " ^ j ^ "] = mem[fp - " ^ k ^ "];\n") (i+1)
	in helper len "" 0;;  

(* Returns code for jumptable *)
let jumpTable fn = 
	let rec helper fn l str i = let j = string_of_int i in 
								if i > l then str
								else helper fn l (str ^ "case " ^ j ^ ":\ngoto L" ^ j ^ "; break;\n") (i+1);
in ignore(append fn ("goto L0;\njumpTable:;\nswitch(r1){\n" ^ helper fn fn.labeltbl "" 1 ^ "}\n"));; 