(* Peter Hess - C Translator in Ocaml - A7 *)
open Scanner;;
open Hashtbl;;
open Function;;

let current_tok = ref ("", Scanner.Error);;
let file = Stream.of_channel (open_in Sys.argv.(1));; (* pass file as command-line argument *)

type func_count = {mutable i: int};;
let addfunc f = f.i <- f.i + 1;;
let func_cnt = {i = 0};;

let localsizes = Hashtbl.create 10;;
let paramsizes = Hashtbl.create 10;;
let out = new_func ();;

current_tok := (Scanner.next_token file);;
let currtoken () = (match !current_tok with (s, _) -> s);;
let currtype () = (match !current_tok with (_, t) -> t);;

while (currtype () = Scanner.Comment) do 
    current_tok := (Scanner.next_token file);
done;;

let tok_match_t toMatch =
  if (currtype () = toMatch) then (
      current_tok := (Scanner.next_token file);
  ) else ();;

let tok_match_s toMatch =
  if (currtoken () = toMatch) then (
      current_tok := (Scanner.next_token file);
  ) else ();;

let rec program () =
  let vl = currtoken () in (
    if (vl = "void") then (
      let fn = Function.new_func () in
      tok_match_s "void"; 
      let id = currtoken () in 
      tok_match_t Scanner.Id;
      tok_match_s "(";
      ignore(parameter_list fn);
      tok_match_s ")";
      let f = func_tail fn in 
      ignore(if not f.isPrototype then (addfunc func_cnt; Hashtbl.add localsizes func_cnt.i f.vars; Hashtbl.add paramsizes id (num_params f)) else ());
      func_list fn;
    ) else if (vl = "int") then ( 
      tok_match_s "int";
      let id = currtoken () in 
      tok_match_t Scanner.Id;
      program_tail id;
    ) else localsizes;
  );
and program_tail (id) =
  let vl = currtoken () in(
    if (vl = "(") then ( 
      tok_match_s "(";
      let fn = Function.new_func () in
      ignore(parameter_list (fn)); 
      tok_match_s ")";
      let f = func_tail fn in 
        if (not f.isPrototype) then (addfunc func_cnt; Hashtbl.add localsizes func_cnt.i f.vars; Hashtbl.add paramsizes id (num_params f)) else ();
        func_list fn;
    ) else (  
      ignore(new_named_var out id); 
      program_decl_tail ();
    )
  );
and program_decl_tail () =
  let vl = currtoken () in (
    if (vl = ",") then ( 
      ignore(tok_match_s ",");
 	    ignore(id_list (out)); 
      ignore(tok_match_s ";");
      program ();
    ) else (
      ignore(tok_match_s ";");
      program ();
    )
  );
and func_list (fn) =
  let vl = currtoken () in(
    if (vl = "int" || vl = "void") then (
      func fn;
      func_list fn;
    )else(
      localsizes;
    )
  );
and func (fn) = 
  let vl = currtoken () in (
    if (vl = "int" || vl = "void") then (
      let fn = Function.new_func () in 
      ignore(func_decl (fn));
      let f = func_tail (fn) in 
      if (not f.isPrototype) then (addfunc func_cnt; Hashtbl.add localsizes func_cnt.i f.vars; Hashtbl.add paramsizes f.header (num_params f)) else ();
    ) else raise (Failure "Parse error - func");
  );
and func_tail (fn) =
  let vl = currtoken () in (
    if (vl = ";") then (
      tok_match_s ";";
      ignore(set_prototype fn);
      fn
    ) 
    else (
       tok_match_s "{";
       let f = (Function.append (statements (data_decls (fn)) "" "") "}\n") in 
       tok_match_s "}";
       f;
    )
  );
and func_decl (fn) =
  let vl = currtoken () in (
    if (vl = "int" || vl = "void") then(
      type_name (fn);
      let id = currtoken () in 
      set_header fn id;
      tok_match_t Scanner.Id;
      tok_match_s "(";
      ignore (parameter_list fn);
      tok_match_s ")";
  )
  else raise (Failure ("Parsing error - func_decl" ^ vl));  );
and type_name (fn) = 
  let vl = currtoken () in
    if (vl = "int") then (tok_match_s "int") else (tok_match_s "void");
and parameter_list (fn) = 
  let vl = currtoken () in (
   	if (vl = "void") then ( 
      tok_match_s "void";
      "void"
    ) 
    else if (vl = "int") then (  
      tok_match_s "int";
      let id = currtoken () in
	    tok_match_t Scanner.Id;
      ignore(add_param fn id); 
      "int " ^ id ^ parameter_list_tail fn;
    ) else (
      ""
    )
  );
and parameter_list_tail (fn) = 
  let vl = currtoken () in (
    if (vl = ",") then (
      tok_match_s ",";
      tok_match_s "int";
      let id = currtoken () in
	    tok_match_t Scanner.Id;
      ignore(add_param fn id);
      ",  int " ^ id ^ parameter_list_tail (fn);
    ) else (
      "";
    )
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
and id_list (fn) = 
  let typ = currtype () in (
    if (typ = Scanner.Id) then (
      let id = currtoken () in 
      tok_match_t Scanner.Id;
      ignore(new_named_var fn id);
      id_list_tail (id_tail fn);
    )else fn;
  );
and id_list_tail (fn) = 
  let vl = currtoken () in (
    if (vl = ",") then (
      tok_match_s ",";
      id_list fn;
    ) else (fn;)
  );
and id_tail (fn) = 
  let vl = currtoken () in (
    if (vl = "[") then (
	   (* doesn't need to support arrays
	   tok_match_s "[";
	   expression ();
	   tok_match_s "]";
	   *) 
      fn;
    ) else(
      fn;
    )
  );
and block_statements fn brk_label cont_label =
  let vl = currtoken () in (
    if (vl = "{") then ( 
      tok_match_s "{";
      ignore(statements fn brk_label cont_label);
      tok_match_s "}"; 
      fn;
    ) else fn;
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
  let vl = currtoken () in(
    if (vl = "(") then ( 
      general_func_call fn id
    ) else(
      assignment fn id
    )
  );
and general_func_call fn id =
  let vl = currtoken () in
  (
    if (vl = "(") then (
      tok_match_s "(";
      let exprlst = expr_list fn in 
      tok_match_s ")";
      tok_match_s ";";
      Function.append fn (id ^ "(" ^ exprlst ^ ");\n");
    )else fn;
  );
and assignment fn id =
  let vl = currtoken () in
  (
    if (vl = "[" || vl = "=") then (
        tok_match_s "=";
        ignore(expression (fn));
        tok_match_s ";";
        fn;
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
and expr_list (fn) = 
  let typ = currtype () and vl = currtoken () in (
    if (typ = Scanner.Id || typ = Scanner.Number || vl = "-" || vl = "(") then ( 
      let result = expression fn in
      result ^ (expr_list_tail fn);
    ) else ("")
  );
and expr_list_tail (fn) = 
  let vl = currtoken () in (
    if (vl = ",") then ( 
      tok_match_s ",";
      let result = ", " ^ (expression fn) in 
      result ^ (expr_list_tail fn);
    ) else ("")
  );
and if_statement fn brk_label cont_label =
  let vl = currtoken () in (
    if (vl = "if") then ( 
      tok_match_s "if";
      tok_match_s "(";
      let labels = condition_expression fn (new_label fn) (new_label fn) in 
      tok_match_s ")";
      ignore(block_statements fn brk_label cont_label);
      else_statement fn labels brk_label cont_label;
    ) else fn;
  );
and else_statement fn labels brk_label cont_label = 
  let vl = currtoken () in (
    if (vl = "else") then (
      ignore(new_label fn);
      tok_match_s "else";
      ignore(block_statements fn brk_label cont_label);
      fn;
    ) else (fn;)
  );
and condition_expression fn true_label false_label = 
  let typ = currtype () and vl = currtoken () in (
    if (typ = Scanner.Id || typ = Scanner.Number || vl = "-" || vl = "(") then (
      let cond_left = condition fn in 
      ignore(condition_expression_tail fn cond_left true_label false_label);
      [|true_label; false_label|];
    ) else raise (Failure "Parsing error - cond expr");
  );
and condition_expression_tail fn cond_left true_label false_label = 
  let vl = currtoken () in (
    if (vl = "&&") then (
      tok_match_s "&&";
      ignore(new_local_var fn);
      ignore(new_local_var fn);
      ignore(new_local_var fn);
      ignore(new_local_var fn);
      ignore(condition fn);
    ) 
    else if (vl = "||") then (
      tok_match_s "||";
      ignore(condition fn);
    )
    else ()
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
      let beginS = new_label fn in 
      tok_match_s "while";
      tok_match_s "(";
      let labels = condition_expression fn (new_label fn) (new_label fn) in
      let nextS = labels.(1) in
      tok_match_s ")";
      ignore(block_statements fn nextS beginS);
      fn;
    )else (fn);
  );
and return_statement (fn) = 
  (
      tok_match_s "return";
      return_statement_tail (fn);
  );
and return_statement_tail (fn) =
  let typ = currtype () and vl = currtoken () in (
    if (typ = Scanner.Id || typ = Scanner.Number || vl = "-" || vl = "(") then (
      ignore(expression (fn));
      tok_match_s ";";
      fn;
    ) else (
      tok_match_s ";";
      fn;
    )
  );
and break_statement fn brk_label =
  let vl = currtoken () in (
    if (vl = "break") then (
      tok_match_s "break";
      tok_match_s ";";
      fn;
    ) else fn;
  );
and continue_statement fn cont_label = 
  let vl = currtoken () in (
    if (vl = "continue") then (
      tok_match_s "continue";
      tok_match_s ";";
      fn;
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
      ignore(new_local_var fn); 
      current_tok := (Scanner.next_token file);
      ignore(term fn);
      (expression_tail fn "");
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
      ignore(new_local_var fn); 
      current_tok := (Scanner.next_token file);
      ignore(factor fn);
      (term_tail fn "");
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
      let place = new_local_var fn in
      tok_match_t Scanner.Number;
      place;
    ) else if (vl = "-") then (
      tok_match_s "-";
      let place = new_local_var fn in 
        tok_match_t Scanner.Number;
        place;
    ) else if (vl = "(") then (
      tok_match_s "(";
      let place = new_local_var fn in 
      ignore(expression fn);
      tok_match_s ")";
      place;
    )else( raise (Failure "Parsing error - factor");)
  );
and factor_tail fn id = 
  let vl = currtoken () in (
    if (vl = "[") then (
      "";
    ) 
    else if (vl = "(") then (
      let place = new_local_var fn in 
      tok_match_s "(";
      ignore(expr_list fn);
      tok_match_s ")";
      place;
    ) 
    else if (vl = "*" || vl = "/" || vl = "+" || vl = "-" || vl = "]" || vl = ")" || vl = ";" || vl = "==" || vl = "!=" || vl = ">" || vl = ">=" || vl = "<" || vl = "<=" || vl = "," || vl = "&&" || vl = "||") then (
      ""
    ) else( raise (Failure "Parsing error - ft");)
);;

program ();;