(* Peter Hess - C Compiler in Ocaml - A8 *)
open Printf;;
open String;;
open Hashtbl;;
open Scanner;;
open Function;;
open Translator;;

Translator.program ();;
let localsize = Translator.localsizes;;
let paramsize = Translator.paramsizes;;
(*Hashtbl.iter (fun x y -> Printf.printf "%d -> %d\n" x y) localsize;;
Hashtbl.iter (fun x y -> Printf.printf "%s -> %d\n" x y) paramsize;;*)

let current_tok = ref ("", Scanner.Error);;
let file = Stream.of_channel (open_in Sys.argv.(1));; 
let output = new_func ();;

type func_count = {mutable i: int};;
let addfunc f = f.i <- f.i + 1;; (* increments function counter *)
let func_cnt = {i = 0};; (* for indexing into localsize *)
let get_local_size () = string_of_int (Hashtbl.find localsize func_cnt.i);;
let get_num_params id = string_of_int (Hashtbl.find paramsize id);;

current_tok := (Scanner.next_token file);;
let currtoken () = match !current_tok with (s, _) -> s;;
let currtype () = match !current_tok with (_, t) -> t;;

while (currtype () = Scanner.Comment) do
    add_comment output (currtoken () ^ "\n");
    current_tok := (Scanner.next_token file); (*copy comments*)
done;;

Function.append output "int fp;\nint sp;\nint r1;\nint mem[2000];\n";; (* fp = frame pointer, sp = stack ptr, r1 = jumpReg *)

let tok_match_t toMatch =
  if (currtype () = toMatch) then (
      current_tok := (Scanner.next_token file);
  ) else ();;

let tok_match_s toMatch =
  if (currtoken () = toMatch) then (
      current_tok := (Scanner.next_token file);
  ) else ();;

(* Modified: eliminate local/global array decls, add main func decl *)
let rec program () =
  let vl = currtoken () in (
    if (vl = "void") then (
      tok_match_s "void";
      ignore(let s = string_of_int output.vars in Function.append output ("int main(){\nsp = " ^ s ^ ";\nfp = " ^ s ^ ";\ngoto mainFunc;"));
      let fn = Function.new_func () in
      let id = currtoken () in 
      tok_match_t Scanner.Id;
      tok_match_s "(";
      ignore(parameter_list fn);
      tok_match_s ")";
      let header = "\n" ^ id ^ "Func:;\n" in
      set_header fn id;
      let f = func_tail fn in 
        let code =  if (f.isPrototype) then ""
                    else (addfunc func_cnt; (header ^ "\nfp = sp;\nsp = fp + " ^ get_local_size () ^ ";\n" ^ (paramset (num_params f)) ^ (get_code f)))
        in 
        ignore(Function.append output (code ^ (func_list fn)));
        jumpTable output; 
        append output "L0:;\n}"; 
    ) 
    else if (vl = "int") then ( 
      tok_match_s "int";
      let id = currtoken () in 
      tok_match_t Scanner.Id;
      (program_tail id);
    ) 
    else (jumpTable output; append output "L0:;\n}")
  );
and program_tail id =
  let vl = currtoken () in (
    if (vl = "(") then (
      tok_match_s "(";
      ignore(let s = string_of_int output.vars in Function.append output ("int main(){\nsp = " ^ s ^ ";\nfp = " ^ s ^ ";\ngoto mainFunc;")); (*s = num of global vars*)
      let fn = Function.new_func () in
      ignore(parameter_list fn);
      tok_match_s ")";
      let header = "\n" ^ id ^ "Func:;\n" in 
      set_header fn id;
      let f = func_tail fn in 
        let code =  if (f.isPrototype) then "" 
                    else (addfunc func_cnt; (header ^ "\nfp = sp;\nsp = fp + " ^ get_local_size () ^ ";\n" ^ (paramset (num_params f)) ^ (get_code f)))
        in 
        ignore(Function.append output (code ^ (func_list fn)));
        jumpTable output; 
        append output "L0:;\n}"; 
    ) else (
      ignore(new_named_var output id); (* add global variable *)
      program_decl_tail ();
    )
  );
and program_decl_tail () =
  let vl = currtoken () in (
    if (vl = ",") then (
      tok_match_s ",";
 	    ignore(id_list (output)); (* add global variable *)
      tok_match_s ";";
      program ();
    ) else (
      tok_match_s ";";
      program ();
    )
  );
and func_list (fn) =
  let vl = currtoken () in(
    if (vl = "int" || vl = "void") then (
      let s = func fn in
      s ^ func_list fn;
    ) else "";
  );
and func (fn) =
  let vl = currtoken () in (
    if (vl = "int" || vl = "void") then (
      let fn = Function.new_func () in (* declare new function here *)
      let id = func_decl fn in
      let header = "\n" ^ id ^ "Func:;\n" in
      set_header fn id;
      let f = func_tail fn in 
      if (f.isPrototype) then ("") (* Modified: if prototype then ignore, else add functionLabel, call paramset, append rest of function code*)
      else(addfunc func_cnt; (header ^ "\nfp = sp;\nsp = fp + " ^ get_local_size () ^ ";\n" ^ (paramset (num_params f)) ^ (get_code f)));
    ) else raise (Failure "Parse error - func");
  );
and func_tail (fn) =
  let vl = currtoken () in (
    if (vl = ";") then (
      tok_match_s ";"; 
      set_prototype fn; (* mark as prototype *)
    ) 
    else (
       tok_match_s "{";
       let f = (Function.append (statements (data_decls fn) "" "") "\n") in
       tok_match_s "}";
       f;
    )
  );
and func_decl (fn) = (* returns id/stores parameters in symbol table *)
  let vl = currtoken () in (
    if (vl = "int" || vl = "void") then (
      ignore(type_name fn); (* ignore return type *)
      let id = currtoken () in 
      tok_match_t Scanner.Id;
      tok_match_s "(";
      ignore(parameter_list fn);
      tok_match_s ")";
      id;
    ) else raise (Failure ("Parsing error - func_decl" ^ vl));  
);
and type_name fn = 
  let vl = currtoken () in
    if (vl = "int") then (tok_match_s "int"; "int ";) else (tok_match_s "void"; "void ";);
and parameter_list fn = (* parses parameter list, stores parameters in symbol table. In translator, returned the parameter list, but now the return value is useless*)
  let vl = currtoken () in (
   	if (vl = "void") then ( 
      tok_match_s "void";
      ""
    ) 
    else if (vl = "int") then (
      tok_match_s "int";
      let id = currtoken () in
	    tok_match_t Scanner.Id;
      ignore(add_param fn id); (*add to parameter list*)
      parameter_list_tail fn;
    ) else ("")
  );
and parameter_list_tail fn = 
  let vl = currtoken () in (
    if (vl = ",") then (
      tok_match_s ",";
      tok_match_s "int";
      let id = currtoken () in
	    tok_match_t Scanner.Id;
      ignore(add_param fn id); (*add to parameter list*)
      parameter_list_tail fn;
    ) else ("";)
  );
and data_decls (fn) = 
  let vl = currtoken () in (
    if (vl = "int") then (
      tok_match_s "int";
      ignore(id_list fn);
      tok_match_s ";";
      data_decls fn;
    ) else (fn;)
  );
and id_list (fn) = (*responsible for adding named local vars to symbol table*)
  let typ = currtype () in (
    if (typ = Scanner.Id) then (
      let id = currtoken () in 
      tok_match_t Scanner.Id;
      ignore(new_named_var fn id);
      id_list_tail (fn);
    )else fn;
  );
and id_list_tail (fn) =
  let vl = currtoken () in (
    if (vl = ",") then (
      tok_match_s ",";
      id_list fn;
    ) else (fn;)
  );
  (* and id_tail (fn) =
  let vl = currtoken () in ( (*array support*)
    if (vl = "[") then ( ) else( )
  ); *)
and block_statements fn brk_label cont_label =
  let vl = currtoken () in (
    if (vl = "{") then (
      tok_match_s "{";
      ignore(statements fn brk_label cont_label);
      tok_match_s "}"; 
      fn;
    )else fn;
  );
and statements fn brk_label cont_label =
  let typ = currtype () and vl = currtoken () in (
    if (typ = Scanner.Id || vl = "break" || vl = "if" || vl = "return" || vl = "while" || vl = "continue" || vl = "scanf" || vl = "printf") then (
      statements (statement (fn) brk_label cont_label) brk_label cont_label;
    ) else (
	   fn;
    )
  );
and statement fn  brk_label cont_label =
  let vl = currtoken () in (
    if (vl = "break") then (
      break_statement fn brk_label;
    ) else if (vl = "continue") then (
      continue_statement fn cont_label;
    ) else if (vl = "if") then (
      if_statement fn brk_label cont_label;
    ) else if (vl = "printf") then (
      printf_func_call fn;
    ) else if (vl = "return") then (
      return_statement fn;
    ) else if (vl = "scanf") then (
      scanf_func_call fn;
    ) else if (vl = "while") then (
      while_statement fn;
    ) else (
      let id = currtoken () in
      tok_match_t Scanner.Id;
      statement_tail fn id;
    )
  );
and statement_tail fn id =
  let vl = currtoken () in (
    if (vl = "(") then (
      general_func_call fn id
    ) else(
      assignment fn id
    )
  );
and general_func_call fn id = 
(* Removed explicit function calls / special cases: read and write *)
  let vl = currtoken () in(
    if (vl = "(") then (
      tok_match_s "(";
      let el = expr_list fn and ret = new_label output in 
      tok_match_s ")";
      tok_match_s ";";
      if (id = "read" || id = "write") then (let exp = (String.concat ", " el) in Function.append fn (id ^ "(" ^ exp ^ ");\n")) 
      else (Function.append fn (pushparams (el) ^ "sp = sp + " ^ get_num_params id ^ ";\nmem[sp] = fp;\nmem[sp+1] = " ^ prev_label_num output ^ ";\nsp = sp + 3; goto " ^ id ^ "Func;\n" ^ ret ^ ":;\nfp = mem[sp - 3];\n" ^ "sp = fp + " ^ get_local_size () ^ ";\n"))
    )else fn;
  );
and assignment fn id =
  let vl = currtoken () in (
    if (vl = "[" || vl = "=") then (
      let place = (if (Function.include_id fn id) then ("mem[fp + " ^ string_of_int (Function.id_to_num fn id) ^ "]") else ("mem[" ^ string_of_int (Function.id_to_num output id) ^ "]")) in
        tok_match_s "=";
        let result = expression fn in 
        tok_match_s ";";
        Function.append fn ("" ^ place ^ " = " ^ result ^ ";\n");
    )else raise (Failure "Parsing error - assignment");
  );
and printf_func_call (fn) =
  let vl = currtoken () in (
    if (vl = "printf") then (
      tok_match_s "printf";
      tok_match_s "(";
      let str = currtoken () in 
      tok_match_t Scanner.String;
      printf_func_call_tail fn str;
    )else fn;
  );
and printf_func_call_tail fn str =
  let vl = currtoken () in (
    if (vl = ")") then ( 
      tok_match_s ")";
      tok_match_s ";";
      Function.append fn ("printf(" ^ str ^ ");\n");
    ) else (
      tok_match_s ",";
      let result = expression (fn) in 
      tok_match_s ")";
      tok_match_s ";";
      Function.append fn ("printf(" ^ str ^ ", " ^ result ^ ");\n");
    )
  );
and scanf_func_call (fn) =
  let vl = currtoken () in (
    if (vl = "scanf") then (
      tok_match_s "scanf";
      tok_match_s "(";
      let str = currtoken () in
      tok_match_t Scanner.String;
      tok_match_s ",";
      tok_match_s "&";
      let result = expression fn
      in
      tok_match_s ")";
       Function.append fn ("scanf(" ^ str ^ ", " ^ result ^ ");\n");
    )else fn;
  );
and expr_list (fn) = (* returns list of expression locations *)
  let typ = currtype () and vl = currtoken () in (
    if (typ = Scanner.Id || typ = Scanner.Number || vl = "-" || vl = "(") then (
      let result = expression fn in
      [result] @ (expr_list_tail fn);
    ) else (
      []
    )
  );
and expr_list_tail (fn) = 
  let vl = currtoken () in (
    if (vl = ",") then (
      tok_match_s ",";
      let result = expression fn in 
      [result] @ (expr_list_tail fn);
    ) else (
      []
    )
  );
and if_statement fn brk_label cont_label =
  let vl = currtoken () in (
    if (vl = "if") then (
      tok_match_s "if";
      tok_match_s "(";
      let labels = condition_expression fn (new_label output) (new_label output) in 
      tok_match_s ")";
		  ignore(Function.append fn (labels.(0) ^ ":;\n") );
      ignore(block_statements fn brk_label cont_label);
      else_statement fn labels brk_label cont_label;
    ) else fn;
  );
and else_statement fn labels brk_label cont_label = 
  let vl = currtoken () in (
    if (vl = "else") then (
      let next_statement = new_label output in 
      tok_match_s "else";
      ignore(Function.append fn ("goto " ^ next_statement ^ ";\n"));
      ignore(Function.append fn (labels.(1) ^ ":;\n") );
      ignore(block_statements fn brk_label cont_label);
      Function.append fn (next_statement ^ ":;");
    ) else (
      Function.append fn (labels.(1) ^ ":;\n");
    )
  );
and condition_expression fn true_label false_label = 
  let typ = currtype () and vl = currtoken () in (
    if (typ = Scanner.Id || typ = Scanner.Number || vl = "-" || vl = "(") then (
      let cond_left = condition fn in 
      ignore(condition_expression_tail fn cond_left true_label false_label);
      [|true_label; false_label|];

    )else raise (Failure "Parsing error - cond expr");
  );
and condition_expression_tail fn cond_left true_label false_label = 
  let vl = currtoken () in (
    if (vl = "&&") then (
      tok_match_s "&&";
      let place = new_local_var fn and not_place = new_local_var fn in 
      ignore(Function.append fn (place ^ " = (" ^ cond_left ^ ");\n"));
      ignore(Function.append fn (not_place ^ " = !" ^ place ^ ";\n"));
      ignore(Function.append fn ("if(" ^ not_place ^ ")" ^ " goto " ^ false_label ^ ";\n"));
      let place2 = new_local_var fn and not_place2 = new_local_var fn and cond = condition fn in
      ignore(Function.append fn (place2 ^ " = (" ^ cond ^ ");\n"));
      ignore(Function.append fn (not_place2 ^ " = !" ^ place2 ^ ";\n"));
      ignore(Function.append fn ("if(" ^ not_place2 ^ ")" ^" goto " ^ false_label ^ ";\n"));
      Function.append fn ("goto " ^ true_label ^ ";\n");
    ) 
    else if (vl = "||") then (
      tok_match_s "||";
      ignore(Function.append fn ( "if(" ^ cond_left ^ ") goto " ^ true_label ^ ";\n" ) );
      let cond = condition fn in Function.append fn ("if(" ^ cond ^ ") goto " ^ true_label ^ ";\ngoto " ^ false_label ^ ";\n"); 
    )
    else (
      Function.append fn ("if(" ^ cond_left ^ ") goto " ^ true_label ^ ";\ngoto " ^ false_label ^ ";\n")
    )
  );
and condition (fn) : string = 
  let typ = currtype () and vl = currtoken () in (
    if (typ = Scanner.Id || typ = Scanner.Number || vl = "-" || vl = "(") then (
      let e1_place = expression fn and c_op = currtoken () in 
      tok_match_t Scanner.Symbol;
      let e2_place = expression fn in 
      ("" ^ e1_place ^ " " ^ c_op  ^ " " ^ e2_place);
    )else raise (Failure "Parsing error - condition");
  );
and while_statement fn = 
  let vl = currtoken () in (
    if (vl = "while") then (
      let beginS = new_label output in 
      ignore(Function.append fn (beginS ^ ":;\n"));
      tok_match_s "while";
      tok_match_s "(";
      let labels = condition_expression fn (new_label output) (new_label output) in
      let nextS = labels.(1) in
      tok_match_s ")";
      ignore(Function.append fn (labels.(0) ^ ":;\n"));
      ignore(block_statements fn nextS beginS);
      Function.append fn ("goto " ^ beginS ^ ";\n" ^ nextS ^ ":;\n");
    )else fn;
  );
and return_statement (fn) =
  (
      tok_match_s "return";
      return_statement_tail (fn);
  );
and return_statement_tail (fn) = 
  let typ = currtype () and vl = currtoken () in ( 
    if (typ = Scanner.Id || typ = Scanner.Number || vl = "-" || vl = "(") then (
      let result = expression (fn) in 
      tok_match_s ";";
      (* if in main, then just return normally *)
      if (fn.header = "main") then Function.append fn ("return " ^ result ^ ";\n")
      else Function.append fn ("mem[fp - 1] = " ^ result ^ ";\nsp = fp;\nr1 = mem[fp-2]; goto jumpTable;")
    ) else (
      tok_match_s ";";
      if (fn.header = "main") then fn
      else Function.append fn ("sp = fp;\nr1 = mem[fp-2]; goto jumpTable;")
    )
  );
and break_statement fn brk_label =
  let vl = currtoken () in (
    if (vl = "break") then (
      tok_match_s "break";
      tok_match_s ";";
      Function.append fn ("goto " ^ brk_label ^ ";\n");
    )else fn;
  );
and continue_statement fn cont_label = 
  let vl = currtoken () in (
    if (vl = "continue") then (
      tok_match_s "continue";
      tok_match_s ";";
      Function.append fn ("goto " ^ cont_label ^ ";\n");
    )
  else fn;
  );
and expression (fn) = 
  let typ = currtype () and vl = currtoken () in (
    if(typ = Scanner.Id || typ = Scanner.Number || vl = "-" || vl = "(") then (
      (expression_tail fn (term fn));
    )
  else( raise (Failure "Parsing error - expr");)
  );
and expression_tail fn e1_left =
  let vl = currtoken () in (
    if(vl = "+" || vl = "-") then (
      let e1_place = new_local_var fn in 
      current_tok := (Scanner.next_token file);
      ignore (Function.append fn (e1_place ^ " = " ^ e1_left ^ vl ^ term fn ^ ";\n"));
      (expression_tail fn e1_place);
    ) else if (vl = "]" || vl = ")" || vl = ";" || vl = "==" || vl = "!=" || vl = ">" || vl = ">=" || vl = "<" || vl = "<=" || vl = "," || vl = "&&" || vl = "||") then (
      e1_left;
    )else( raise (Failure "Parsing error - expr_tail");)
  );
and addop fn =
  let vl = currtoken () in (
    if (vl = "+") then (
      tok_match_s "+"; 
      "+";
    ) else if (vl = "-") then (
      tok_match_s "-"; 
      "-";
    )else( raise (Failure ("Parsing error - addop" ^ vl));)
  );
and term fn = 
  let typ = currtype () and vl = currtoken () in (
    if (typ = Scanner.Id || typ = Scanner.Number || vl = "-" || vl = "(") then (
      term_tail fn (factor fn);
    )else raise (Failure "Parsing error - term");
  );
and term_tail fn t1_left = 
  let vl = currtoken () in (
    if (vl = "*" || vl = "/") then (
      let t1_place = new_local_var fn in 
      current_tok := (Scanner.next_token file);
      ignore (Function.append fn (t1_place ^ " = " ^ t1_left ^ vl ^ (factor fn) ^ ";\n"));
      (term_tail fn t1_place);
    ) else if (vl = "+" || vl = "-" || vl = "]" || vl = ")" || vl = ";" || vl = "==" || vl = "!=" || vl = ">" || vl = ">=" || vl = "<" || vl = "<=" || vl = "," || vl = "&&" || vl = "||") then (
      t1_left;
    )else( raise (Failure ("Parsing error - tt ")); )
  );
and mulop fn = 
  let vl = currtoken () in (
    if (vl = "*") then (
      tok_match_s "*"; 
      "*";
    ) else if (vl = "/") then (
      tok_match_s "/"; 
      "/";
    )else( raise (Failure "Parsing error - mulop");)
  );
and factor (fn) = 
  let typ = currtype () and vl = currtoken () in (
    if (typ = Scanner.Id) then (
      let id = currtoken () in
      tok_match_t Scanner.Id;
      factor_tail fn id;
    ) else if (typ = Scanner.Number) then (
      let num = currtoken () and place = new_local_var fn in
      tok_match_t Scanner.Number;
      ignore (Function.append fn (place ^ " = " ^ num ^ ";\n"));
      place;
    ) else if (vl = "-") then (
      tok_match_s "-";
      let place = new_local_var fn and num = currtoken () in 
        tok_match_t Scanner.Number;
        ignore (Function.append fn (place ^ " = -" ^ num ^ ";\n"));
        place;
    ) else if (vl = "(") then (
      tok_match_s "(";
      let place = new_local_var fn in 
      ignore(Function.append fn (place ^ " = " ^ expression (fn) ^ ";\n" ));
      tok_match_s ")";
      place;
    )else( raise (Failure "Parsing error - factor");)
  );
and factor_tail fn id = 
  let vl = currtoken () in (
    if (vl = "[") then ("";) (* No array support *) 
    else if (vl = "(") then (
      let place = new_local_var fn in 
      tok_match_s "(";
       (* Modified: call pushparams; sp = sp + num params; mem[sp] = fp; mem[sp+1] = returnlabel; sp = sp + 3; goto idFunc; Li:; fp = mem[sp-3]; 
        store mem[sp-1] to place, sp = fp + get_local_size () *)
      let code = pushparams (expr_list fn) and ret = new_label output in 
      ignore(Function.append fn (code ^ "sp = sp + " ^ get_num_params id ^ ";\nmem[sp] = fp;\nmem[sp+1] = " ^ prev_label_num output ^ ";\nsp = sp + 3; goto " ^ id ^ "Func;\n" ^ ret ^ ":;\nfp = mem[sp - 3];\n" ^ place ^ " = mem[sp - 1];\nsp = fp + " ^ get_local_size ()  ^ ";\n"));
      tok_match_s ")";
      place;
    ) else if (vl = "*" || vl = "/" || vl = "+" || vl = "-" || vl = "]" || vl = ")" || vl = ";" || vl = "==" || vl = "!=" || vl = ">" || vl = ">=" || vl = "<" || vl = "<=" || vl = "," || vl = "&&" || vl = "||") then (
      if (include_id fn id) then ("mem[fp + " ^ (string_of_int (id_to_num fn id)) ^ "]") (* Modified: use mem *)
      else ("mem[" ^ (string_of_int (id_to_num output id)) ^ "]")
    )
  else( raise (Failure "Parsing error - ft");)
);;

program ();;
printf ((get_comments output) ^ (get_code output));; (*OUTPUT to stdout*)
(*OUTPUT TO FILE let () = let fp = open_out "output.txt" in fprintf fp "%s" ((get_comments output) ^ (get_code output)); close_out fp;; *)