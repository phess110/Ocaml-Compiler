(* Peter Hess - C Scanner in Ocaml - A7 *)
open List;;
open Printf;;
open String;;

type tokenType = Comment | String | Id | Symbol | Number | Error | Reserved | EOF;;
type token = (string * tokenType);;
type dfa = {
	last_state: int;
	current_state: int;
	transition_function: (int * char * int) list;
	accepting_states: int list;
	token: string;
};;

let nums = ['1';'2';'3';'4';'5';'6';'7';'8';'9';'0'];;
let letters = ['_';'a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z';'A';'B';'C';'D';'E';'F';'G';'H';'I';'J';'K';'L';'M';'N';'O';'P';'Q';'R';'S';'T';'U';'V';'W';'X';'Y';'Z'];;
let syms = ['(';')';'+';'-';'*';'[';']';'{';'}';',';';'];;
let symbols = append syms ['/';'!';'=';'&';'|';'>';'<'];;
let whitespace = [' ';'\t'];;
let followsym = append ['(';')'] (append nums (append whitespace letters));;
let alphabet = append whitespace (append symbols (append nums letters));; 
let reservedWords = ["int"; "void"; "if"; "while"; "return"; "else"; "continue"; "break"; "scanf"; "printf";];;

(* for all a in set add transition (s,a,e) to lst *)
let rec to_trans lst set (s:int) (e:int) = match set with
	| [] -> lst
	| h::tl -> (s, h, e) :: (to_trans lst tl s e);;

let start_of_token = let t = to_trans(to_trans(to_trans(to_trans(to_trans(to_trans(to_trans(to_trans [] nums 0 1) nums 1 1) letters 0 2) (append letters nums) 2 2) syms 0 3) (append alphabet ['\n';'%';'.';'\\';':';'?';'\'']) 12 12) (append alphabet ['\"';'.';'%';'\\';':';'?';'\'']) 10 10) (append whitespace ['\n']) 0 0 in
				(12,'\"',13)::(0,'\"',12)::(11,'=',3)::(0,'!',11)::(0,'#',10)::(9,'/',10)::(0,'/',9)::(5,'&',3)::(6,'|',3)::(7,'=',3)::(8,'=',3)::(0,'>',8)::(0,'<',7)::(0,'|',6)::(0,'&',5)::(4,'=',3)::(0,'=',4):: t;;
let end_of_token = let t = to_trans(to_trans(to_trans(to_trans(to_trans(to_trans(to_trans(to_trans(to_trans(to_trans [] (append symbols whitespace) 1 (-1)) (append symbols whitespace) 2 (-1)) (append alphabet ['\n'; '\"']) 3 (-1)) alphabet 13 (-1)) (filter (fun x -> x != '/') alphabet) 9 (-1)) followsym 4 (-1)) followsym 5 (-1)) followsym 6 (-1)) followsym 7 (-1)) followsym 8 (-1) in 
					(10,'\n',-1)::t;;
				
let transitions = append start_of_token end_of_token;; (* define transiton function *)	

let isReserved (str: string) : bool = mem str reservedWords;; (* returns true if str is a keyword *)

let getType (str: string) (state:int) : tokenType = match state with (* returns token_Type of str *)
	| 1 -> Number
	| 2 -> if isReserved str then Reserved else Id
	| 3 | 4 | 5 | 6 | 7 | 8 | 9 -> Symbol
	| 10 -> Comment
	| 13 -> String
	| 0 -> EOF
	| _ -> Error;;

let peekAtStream file = match Stream.peek file 
						with | None -> ' ' 
							 | Some s -> s;;

(* return string representation of next_char from file *)
let next_char file = if (peekAtStream file = '\\') then (ignore(Stream.next file); ignore(Stream.next file); "\\n") else Char.escaped (Stream.next file);;

(* updates dfa state given input *)
let transition (d: dfa) (input: char) file: dfa = 
	let new_state = (let (_,_,q) = find (fun (a,c, _) -> a = d.current_state && c = input) d.transition_function in q) in
	{ 
		last_state = d.current_state;
		current_state = new_state;
		token = if new_state = (-1) then d.token else (if new_state = 0 then (ignore(Stream.next file); d.token) else (d.token ^ (next_char file))); (* Only consume input if it is a valid continuation of current token, deletes whitespace*)
		transition_function = d.transition_function;
		accepting_states = d.accepting_states;
	};;

(* 	
	returns next token = (string, token_Type)
	tokenType = Error if scanning fails, = EOF if at end of file
*)
let next_token file = 
	let scanner : dfa = {
		last_state = 0;
		current_state = 0;
		transition_function = transitions;
		accepting_states = [1;2;3;4;5;6;7;8;9;10;13];
		token = "";
	} in let rec helper d file = 
		try if d.current_state = -1 then (if mem d.last_state d.accepting_states then Some (d.token, d.last_state) else None) else (helper (transition d (peekAtStream file) file) file)
			with Not_found -> None
				| Stream.Failure -> if mem d.current_state d.accepting_states then Some (d.token, d.current_state) else Some ("", 0) in
	match helper scanner file with
		| None -> ("", Error)
		| Some (s,i) -> (trim s, getType s i);;

(* display all tokens in file *)
let rec token_printer file = match next_token file
					with | (_,EOF) -> () 
					| (_, Error) -> printf "ERROR" 
					| (s,t) -> printf "Token: %s\n" s; token_printer file;;

(*token_printer file;;*)