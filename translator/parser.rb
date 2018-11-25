#!/usr/bin/ruby
require 'set'
require_relative 'node'
require_relative 'num'
require_relative 'arrayname'

class Parser
	@input
	@index
	attr_accessor :vars, :numfuncs, :numstas, :root, :debug, :numglob, :globnames

	LETTERS="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

	def initialize(infile)
		@vars=[]
		@numfuncs=0
		@numstas=0
		@index=0
		ifi=File.open(infile,"r")
		@input=ifi.read
		ifi.close
		@input.lstrip!
	end

	def prep_read(currnode=nil,ind)
		while cant_read!=0
			case cant_read
			when 1
				@index+=1
			when 2
				index2=@index
				index2+=(@input[@index,(@input.length-@index)])=~%r{\n}
				@index=index2+1
			when 3
				index2=@index
				comm_str=""
				while @input[index2]!="\n" && @input[index2]!="\r"
					comm_str<<@input[index2]
					index2+=1
				end
				@index=index2+1
				if currnode!=nil
					comm=Node.new
					comm.type="Preprocessor directive (Meta statement)"
					comm.str=comm_str.dup
					currnode.insert(ind,comm)
				end
			else
				puts("Error")
				raise "Something went really wrong"
			end
		end
	end

	def cant_read
		foo=@input[@index]
		if foo==" " || foo=="	" || foo=="\n" || foo=="\r"
			return 1
		elsif @input[@index]=="/" && @input[@index+1]=="/"
			return 2
		elsif @input[@index]=="#"
			return 3
		else
			return 0
		end
	end

	def peek(currnode=nil, ind=-1)
		prep_read(currnode,ind)
		@input[@index]
	end

	def trymatch(term,currnode=nil,ind=-1)
		prep_read(currnode,ind)
		if term == @input[@index,term.length]
			return true
		end
		return false
	end

	def match(term, orthis=nil, currnode=nil, ind=-1)
		prep_read(currnode,ind)
		if term != @input[@index,term.length]
			lns=@input.lines
			lno=0
			chno=@index
			tln=lns[0]
			for ln in lns
				if (ln.length)<=chno
					chno-=(ln.length)
					lno+=1
				else
					tln=ln
					break
				end
			end
			puts("Error")
			wanted=term.dup
			if orthis!=nil
				wanted<<orthis
			end
			if debug
				puts(@input[0,@index+1].dump)
				esclen=@input[0,@index+1].dump.length
				escindex=@index+((esclen)-(@input[0,@index+1].length))
				print(" "*(escindex-1))
				puts("^")
				puts("Parse tree so far:")
				puts(@root)
			end
			spaces=""
			ind=0
			while spaces.length!=chno
				spaces<<tln[ind]=="\t"?"\t":" "
				ind+=1
			end
			raise "Parsing error\nParse error at line %d character %d\nExpected \"%s\".\n%s\n%s^" % [lno,chno,wanted,tln,spaces]
		end
		@index+=term.length
		return true
	end

	def isdigit(char)
		return char=="0" || char=="1" || char=="2" || char=="3" || char=="4" || char=="5" || char=="6" || char=="7" || char=="8" || char=="9"
	end

	def isletter(char)
		return LETTERS.index(char)!=nil
	end

	#Because the grammar is so broken and ambiguous, and there are only so many hours in the day,
	#I'm doing a bad; only for the cases where I REALLY need to, I'm going to pretend
	#that I have infinite lookahead by checking if i can find something before another thing.
	def cheat_lookahead_isthere_before(is_there_a, before_a)
		if @input.index(is_there_a,@index) == nil
			return false
		end
		if @input.index(before_a,@index)==nil
			return true
		end
		@input.index(is_there_a,@index) < @input.index(before_a,@index)
	end

	def start
		@root=program
	end

	#Program -> Data_decls Func_list
	def program
		root=Node.new
		root.type="Program"
		prep_read(root,-1)
		root<<data_decls
		@vars.length.times do |i|
			n=@vars[i]
			if n.is_a?(Arrayname)
				if root.get("Array Names")==nil
					root.misc.store("Array Names",[])
				end
				root.get("Array Names") << n.name
				(n.length-1).times do |j|
					@vars.insert(i+j+1,"&%s+%d" % [n,j+1])
				end
				@vars[i]="&%s+0" % n
			end
		end
		@numglob=@vars.length
		if debug
			puts("Number of global vars: %d" % @numglob)
		end
		root.misc.store("Global Vars",@numglob)
		root.misc.store("Global Var Names",@vars.dup)
		@globnames=@vars.dup
		root<<func_list
		return root
	end

	#Func_list -> EPS | Func Func_list
	def func_list
		this=Node.new
		this.type="Func_list"
		if peek(this)=="i" || peek(this)=="v"
			this << func
			this << func_list
		else
			this.str="EPSILON"
		end
		return this
	end

	#Func -> Func_decl Func_tail
	def func
		this=Node.new
		this.type="Func"
		numparams=Num.new(0)
		startvars=@vars.length
		this<<func_decl(numparams)
		if debug
			puts("number of params: %d" % numparams)
		end
		this<<func_tail(numparams,startvars)
		return this
	end

	#Func_tail -> ; | { Data_decls Statements }
	def func_tail(numparams=0, startvars=0, giveall=false)
		this=Node.new
		this.type="Func_tail"
		numregs=0
		if peek(this)==";"
			match(";",nil,this)
			this.str=";"
		else
			@numfuncs+=1
			numlocals=numparams
			match("{",nil,this)
			newvars=[]
			this << data_decls2(newvars)
			numlocals+=newvars.length
			numloc2=newvars.length
			numloc2.times do |i|
				n=newvars[i]
				if n.is_a?(Arrayname)
					if this.get("Array Names")==nil
						this.misc.store("Array Names",[])
					end
					this.get("Array Names") << n.name
					numloc2+=n.length-1
					numlocals+=n.length-1
					(n.length-1).times do |j|
						newvars.insert(i+j+1,"&%s+%d" % [n,j+1])
					end
					newvars[i]="&%s+0"%n
					raise "lengths don't match. probably off by 1 error.\nnumloc2: %d\bactual length: %d" % [numloc2,newvars.length] unless numloc2==newvars.length
				end
			end
			this << statements
			if debug
				puts("Number of local vars: %d"%numlocals)
			end
			ex,co,nu,fc=count_expr_cond_const_call(this)
			numregs=ex+co+numlocals+fc+nu
			this.misc.store("Local Vars",numloc2)
			this.misc.store("Local Exprs",ex)
			this.misc.store("Local Conds",co)
			this.misc.store("Local Nums",nu)
			this.misc.store("Func Calls",fc)
			this.misc.store("Local Params",numparams)
			this.misc.store("Local Regs",co+ex+numloc2+nu+fc+numparams)
			this.misc.store("Local Var Names",newvars)
			match("}",nil,this)
			this.str="{ }"
		end
		if giveall
			return this,numregs
		end
		return this
	end

	#Func_decl -> Type_name Identifier ( Parameter_list )
	def func_decl(numparams=0)
		this=Node.new
		this.type="Func_decl"
		this<<type_name(this,0)
		this<<identifier
		if debug
			puts("Function %s" % collate_id(this[-1],false))
		end
		match("(",nil,this,0)
		this.str="( )"
		parl=[]
		this<<parameter_list(numparams,parl)
		if debug && !parl.empty?
			puts("Parameter names: %p" % parl)
		end
		this.misc.store("Param Names",parl.dup)
		match(")",nil,this)
		return this
	end

	#Type_name -> i n t | v o i d
	#parent for putting meta-statements in the correct place and myind for putting them in the right index
	def type_name(parent=nil, myind=-1)
		this=Node.new
		this.type="Type_name"
		if peek(parent,myind)=="i"
			match("int"," or void",parent,myind)
			this.str="int"
		else
			match("void", " or int",parent,myind)
			this.str="void"
		end
		return this
	end

	#Parameter_list -> EPS | v o i d | Non_empty_list
	def parameter_list(numparams=0,parl=[])
		this=Node.new
		this.type="Parameter_list"
		if peek(this)==")"
			this.str="EPSILON"
		elsif trymatch("void")
			match("void",nil,this)
			this.str="void"
		else
			this<<non_empty_list(numparams,parl)
		end
		return this
	end

	#Non_empty_list -> Type_name Identifier Param_list_tail
	def non_empty_list(numparams=0,parl=[])
		this=Node.new
		this.type="Non_empty_list"
		this<<type_name
		this<<identifier
		parl<<collate_id(this[-1],false)
		numparams.add(1)
		this<<param_list_tail(numparams,parl)
		return this
	end

	#Param_list_tail -> EPS | , Type_name Identifier Param_list_tail
	def param_list_tail(numparams,parl=[])
		this=Node.new
		this.type="Param_list_tail"
		if peek==")"
			this.str="EPSILON"
		else
			match(",")
			this.str=","
			this<<type_name
			this<<identifier
			parl<<collate_id(this[-1],false)
			numparams.add(1)
			this<<param_list_tail(numparams)
		end
		return this
	end

	#Data_decls -> EPS | Type_name Id_list ; Data_decls
	def data_decls
		this=Node.new
		this.type="Data_decls"
		if !cheat_lookahead_isthere_before(";","(")
			this.str="EPSILON"
		else
			this << type_name(this,0)
			this << id_list(true,[],true,Num.new(0))
			match(";")
			this.str=";"
			this << data_decls
		end
		return this
	end

	#Data_decls2 -> EPS | Type_name Id_list ; Data_decls2
	def data_decls2(addto=[])
		this=Node.new
		this.type="Data_decls2"
		if trymatch("int ") || trymatch("void ")
			this<<type_name(this,0)
			this<<id_list(false,addto,true,Num.new(0))
			match(";")
			this.str=";"
			this<<data_decls2(addto)
		else
			this.str="EPSILON"
		end
		return this
	end

	#Id_list -> Identifier Id_list_tail
	def id_list(isglob=false,addto=[],constarr=false,arrlen=nil)
		this=Node.new
		this.type="Id_list"
		this<<identifier(constarr,arrlen)
		collate_id(this.nodes[0],true,addto,arrlen!=0,arrlen)
		this.nodes[0].misc.store("global",isglob)
		this<<id_list_tail(isglob,addto,constarr,Num.new(0))
		return this
	end

	#Id_list_tail -> EPS | , Identifier Id_list_tail
	def id_list_tail(isglob=false,addto=[],constarr=false,arrlen=nil)
		this=Node.new
		this.type="Id_list_tail"
		if peek==";"
			this.str="EPSILON"
		else
			match(",")
			this.str=","
			this<<identifier(constarr,arrlen)
			collate_id(this.nodes[0],true,addto,arrlen!=0,arrlen)
			this.nodes[0].misc.store("global",isglob)
			this<<id_list_tail(isglob,addto,constarr,Num.new(0))
		end
		return this
	end

	#Identifier -> Id Array_tail
	def identifier(constarr=false,arrlen=nil)
		this=Node.new
		this.type="Identifier"
		this<<id
		this<<array_tail(constarr,arrlen)
		return this
	end

	def collate_id(ident,addvar=true,addto=[],isarr=false,arrlen=nil)
		if !ident.is_a?(Node)
			puts("Error")
			raise "Trying to collate the name of something that is not a node. Type is %s" % ident.class
		end
		if ident.type!="Identifier"
			puts("Error")
			raise "Trying to collate the name of a non-identifier. Type is %s" % ident.type
		end
		name=""
		strt=ident.nodes[0]#Id
		name<<strt.nodes[0].str#Letter
		curr=strt.nodes[1]#Id_tail
		while curr.str!="EPSILON"
			if curr.str!=nil
				name<<curr.str#underscore
			else
				name<<curr.nodes[0].str#Letter or Digit
			end
			curr=curr.nodes[curr.str==nil ? 1 : 0]#Id_tail
		end
		thing=name
		if isarr
			thing=Arrayname.new(name,arrlen.to_i)
		end
		addto<<thing
		if addvar
			@vars<<thing
		else
			return thing
		end
	end

	def collate_num_lit(nd,prefix="")
		if nd.type!="Number"
			raise "Error collating a number.\nPassed node type is %s" % nd.type
		end
		str=prefix
		num=0
		str<<nd[0].str
		num+=nd[0].str[0].ord-48
		curr=nd[1]
		until curr.str=="EPSILON"
			num*=10
			str<<curr[0].str
			num+=curr[0].str[0].ord-48
			curr=curr[1]
		end
		if prefix=="-"
			num*=-1
		end
		return num
	end

	#Array_tail -> EPS | [ Expression ]
	def array_tail(constarr,arrlen=nil)
		this=Node.new
		this.type="Array_tail"
		if peek=="["
			match("[")
			this<<expression
			if (constarr && ( this[-1][0][0][0].type!="Number" || (this[-1][0][0][0].type=="Number" && this[-1][0][0].str=="-")))
				raise "Error: Array declaration must be an positive integer constant"
			end
			if constarr
				num=collate_num_lit(this[-1][0][0][0])
				if arrlen==nil
					arrlen=Num.new(0)
				end
				arrlen.add(num)
			end
			match("]")
			this.str="[ ]"
		else
			this.str="EPSILON"
		end
		return this
	end

	#Block_statements -> { Statements }
	def block_statements
		this=Node.new
		this.type="Block_statements"
		match("{")
		this<<statements
		match("}")
		this.str="{ }"
		return this
	end

	#Statements -> EPS | Statement Statements
	def statements
		this=Node.new
		this.type="Statements"
		if peek=="}"
			this.str="EPSILON"
		else
			this<<statement
			this<<statements
		end
		return this
	end

	#Statement -> Identifier Assig_or_call | Printf_func_call | Scanf_func_call | If_statement | While_statement | Return_statement | Break_statement | Continue_statement
	def statement
		this=Node.new
		this.type="Statement"
		@numstas+=1
		if trymatch("printf(") || trymatch("printf ")
			this<<printf_func_call
		elsif trymatch("scanf(") || trymatch("scanf ")
			this<<scanf_func_call
		elsif trymatch("if(") || trymatch("if ") || trymatch("if	")
			this<<if_statement
		elsif trymatch("while(") || trymatch("while ") || trymatch("while	")
			this<<while_statement
		elsif trymatch("return;") || trymatch("return ")
			this<<return_statement
		elsif trymatch("break;")
			this<<break_statement
		elsif trymatch("continue;")
			this<<continue_statement
		else
			this<<identifier
			this<<assig_or_call
		end
		return this
	end

	#Assig_or_call -> = Expression ; | ( Expr_list ) ;
	def assig_or_call
		this=Node.new
		this.type="Assig_or_call"
		if peek=="="
			match("=")
			this<<expression
			match(";")
			this.str="= ;"
		else
			match("(")
			this<<expr_list
			match(")")
			match(";")
			this.str="( ) ;"
		end
		return this
	end

	#Printf_func_call -> printf ( String Printf_args ) ;
	def printf_func_call
		this=Node.new
		this.type="Printf_func_call"
		match("printf")
		match("(")
		this<<string
		this<<printf_args
		match(")")
		match(";")
		this.str="printf ( ) ;"
		return this
	end

	#Printf_args -> EPS | , Expression
	def printf_args
		this=Node.new
		this.type="Printf_args"
		if peek==")"
			this.str="EPSILON"
		else
			match(",")
			this.str=","
			this<<expression
		end
		return this
	end

	#Scanf_func_call -> scanf ( String , & Expression ) ;
	def scanf_func_call
		this=Node.new
		this.type="Scanf_func_call"
		match("scanf")
		match("(")
		this<<string
		match(",")
		match("&")
		this<<expression
		match(")")
		match(";")
		this.str="scanf ( , & ) ;"
		return this
	end

	#Expr_list -> EPS | Expression Ne_expr_list_tail
	def expr_list
		this=Node.new
		this.type="Expr_list"
		if peek==")"
			this.str="EPSILON"
		else
			this<<expression
			this<<ne_expr_list_tail
		end
		return this
	end

	#Ne_expr_list_tail -> EPS | , Expression Ne_expr_list_tail
	def ne_expr_list_tail
		this=Node.new
		this.type="Ne_expr_list_tail"
		if peek==")"
			this.str="EPSILON"
		else
			match(",")
			this.str=","
			this<<expression
			this<<ne_expr_list_tail
		end
		return this
	end

	#If_statement -> if ( Condition_expression ) Block_statements Else_statement
	def if_statement
		this=Node.new
		this.type="If_statement"
		match("if")
		match("(")
		this<<condition_expression
		match(")")
		this<<block_statements
		this<<else_statement
		this.str="if ( )"
		return this
	end

	#Else_statement -> EPS | else Block_statements
	def else_statement
		this=Node.new
		this.type="Else_statement"
		if trymatch("else ") || trymatch("else\n")
			match("else")
			this<<block_statements
		else
			this.str="EPSILON"
		end
		return this
	end

	#Condition_expression -> Condition Cond_exp_tail
	def condition_expression
		this=Node.new
		this.type="Condition_expression"
		this<<condition
		this<<cond_exp_tail
		return this
	end

	#Cond_exp_tail -> EPS | Condition_op Condition
	def cond_exp_tail
		this=Node.new
		this.type="Cond_exp_tail"
		if peek==")"
			this.str="EPSILON"
		else
			this<<condition_op
			this<<condition
		end
		return this
	end

	#Condition_op -> && | ||
	def condition_op
		this=Node.new
		this.type="Condition_op"
		if peek=="&"
			match("&&")
			this.str="&&"
		else
			match("||")
			this.str="||"
		end
		return this
	end

	#Condition -> Expression Comparison_op Expression
	def condition
		this=Node.new
		this.type="Condition"
		this<<expression
		this<<comparison_op
		this<<expression
		return this
	end

	#Comparison_op -> == | != | > | >= | < | <=
	def comparison_op
		this=Node.new
		this.type="Comparison_op"
		if peek=="="
			match("==")
			this.str="=="
		elsif peek=="!"
			match("!=")
			this.str="!="
		elsif peek=="<"
			if trymatch("<=")
				match("<=")
				this.str="<="
			else
				match("<")
				this.str="<"
			end
		else
			if trymatch(">=")
				match(">=")
				this.str=">="
			else
				match(">")
				this.str=">"
			end
		end
		return this
	end

	#While_statement -> while ( Condition_expression ) Block_statements
	def while_statement
		this=Node.new
		this.type="While_statement"
		match("while")
		match("(")
		this<<condition_expression
		match(")")
		this<<block_statements
		this.str="while ( )"
		return this
	end

	#Return_statement -> return Return_arg ;
	def return_statement
		this=Node.new
		this.type="Return_statement"
		match("return")
		this<<return_arg
		match(";")
		this.str="return ;"
		return this
	end

	#Return_arg -> EPS | Expression
	def return_arg
		this=Node.new
		this.type="Return_arg"
		if peek==";"
			this.str="EPSILON"
		else
			this<<expression
		end
		return this
	end

	#Break_statement -> break;
	def break_statement
		this=Node.new
		this.type="Break_statement"
		match("break;")
		this.str="break;"
		return this
	end

	#Continue_statement -> continue;
	def continue_statement
		this=Node.new
		this.type="Continue_statement"
		match("continue;")
		this.str="continue;"
		return this
	end

	#Expression -> Term Expr_tail
	def expression
		this=Node.new
		this.type="Expression"
		this<<term
		this<<expr_tail
		return this
	end

	#Expr_tail -> EPS | Addop Term Expr_tail
	def expr_tail
		this=Node.new
		this.type="Expr_tail"
		if peek=="+" || peek=="-"
			this<<addop
			this<<term
			this<<expr_tail
		else
			this.str="EPSILON"
		end
		return this
	end

	#Addop -> + | -
	def addop
		this=Node.new
		this.type="Addop"
		if peek=="+"
			match("+")
			this.str="+"
		else
			match("-")
			this.str="-"
		end
		return this
	end

	#Term -> Factor Term_tail
	def term
		this=Node.new
		this.type="Term"
		this<<factor
		this<<term_tail
		return this
	end

	#Term_tail -> EPS | Mulop Factor Term_tail
	def term_tail
		this=Node.new
		this.type="Term_tail"
		if peek=="*" || peek=="/"
			this<<mulop
			this<<factor
			this<<term_tail
		else
			this.str="EPSILON"
		end
		return this
	end

	#Mulop -> * | /
	def mulop
		this=Node.new
		this.type="Mulop"
		if peek=="*"
			match("*")
			this.str="*"
		else
			match("/")
			this.str="/"
		end
		return this
	end

	#Factor -> Identifier Factor_tail | Number | - Number | ( Expression )
	def factor
		this=Node.new
		this.type="Factor"
		if peek=="("
			match("(")
			this<<expression
			match(")")
			this.str="( )"
		elsif peek=="-"
			match("-")
			this.str="-"
			this<<number
		elsif isdigit(peek)
			this<<number
		else
			this<<identifier
			this<<factor_tail
		end
		return this
	end

	#Factor_tail -> EPS | [ Expression ] | ( Expr_list )
	def factor_tail
		this=Node.new
		this.type="Factor_tail"
		if peek=="["
			match("[")
			this<<expression
			match("]")
			this.str="[ ]"
		elsif peek=="("
			match("(")
			this<<expr_list
			match(")")
			this.str="( )"
		else
			this.str="EPSILON"
		end
		return this
	end

	#String -> " <literally anything but "> "
	def string
		this=Node.new
		this.type="String"
		match("\"")
		this.str="\""
		while true
			if @input[@index]=="\""
				if @input[@index-1]=="\\"
					this.str<<"\""
				else
					break
				end
			end
			this.str<<@input[@index]
			@index+=1
		end
		match("\"")
		this.str<<"\""
		return this
	end

	#Number -> Digit Num_tail
	def number
		this=Node.new
		this.type="Number"
		this<<digit
		this<<num_tail
		return this
	end

	#Digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
	def digit
		this=Node.new
		this.type="Digit"
		case peek
		when "0"
			match("0")
			this.str="0"
		when "1"
			match("1")
			this.str="1"
		when "2"
			match("2")
			this.str="2"
		when "3"
			match("3")
			this.str="3"
		when "4"
			match("4")
			this.str="4"
		when "5"
			match("5")
			this.str="5"
		when "6"
			match("6")
			this.str="6"
		when "7"
			match("7")
			this.str="7"
		when "8"
			match("8")
			this.str="8"
		when "9"
			match("9")
			this.str="9"
		else
			puts("Error")
			raise "Something went wrong.\nThere should have been a digit, but instead, I got \"%s\"" % peek
		end
		return this
	end

	#Num_tail -> EPS | Digit Num_tail
	def num_tail
		this=Node.new
		this.type="Num_tail"
		if isdigit(peek)
			this<<digit
			this<<num_tail
		else
			this.str="EPSILON"
		end
		return this
	end

	#Id -> Letter Id_tail
	def id
		this=Node.new
		this.type="Id"
		this<<letter
		this<<id_tail
		return this
	end

	#Id_tail -> EPS | Letter Id_tail | Digit Id_tail | _ Id_tail
	def id_tail
		this=Node.new
		this.type="Id_tail"
		if isletter(peek)
			this<<letter
			this<<id_tail
		elsif isdigit(peek)
			this<<digit
			this<<id_tail
		elsif peek=="_"
			match("_")
			this.str="_"
			this<<id_tail
		else
			this.str="EPSILON"
		end
		return this
	end

	#Letter -> [a-zA-Z]
	def letter
		this=Node.new
		this.type="Letter"
		if isletter(peek)
			this.str=peek
			match(peek)
		else
			puts("Error")
			raise "Expected a letter, but got \"%s\"" % peek
		end
		return this
	end

	def idbputs(thing)
		if debug
			puts(thing)
		end
	end

	def count_expr_cond_const_call(nd)
		q=[]
		exprs=0
		conds=0
		nums=0
		fcalls=0
		if nd.numkids==0
			return 0,0,0,0
		else
			nd.nodes.each do |i|
				q.push(i)
			end
		end
		until q.empty?
			curr=q.shift
			unless curr.nodes.empty?
				curr.nodes.each do |j|
					q.push(j)
				end
			end
			if curr.type=="Expr_tail" && curr.str!="EPSILON"
				exprs+=1
				idbputs("add expr tail")
			elsif curr.type=="Term_tail" && curr.str!="EPSILON"
				conds+=1
				idbputs("add term tail")
			elsif curr.type=="Factor"
				ncurr=curr[0]
				if ncurr.type=="Number" && (ncurr.parent.parent.parent.parent.type!="Array_tail" || ncurr.parent.parent.parent.parent.parent.parent.type=="Statement")
					nums+=1
					idbputs("add number %d"%collate_num_lit(ncurr))
				elsif ncurr.type=="Identifier" && curr[1].str!="EPSILON"
					fcalls+=1
					idbputs("add func call factor")
				elsif ncurr.type=="Identifier" && ncurr[1].str!="EPSILON"
					exprs+=1
					idbputs("add array tail factor")
				end
			elsif curr.type=="Condition"
				conds+=1
				idbputs("add condition")
			elsif curr.type=="Cond_exp_tail" && curr.str!="EPSILON"
				conds+=1
				idbputs("add cond exp tail")
			elsif curr.type=="Statement" && curr[0].type=="Identifier" && curr[0][1].str!="EPSILON"
				exprs+=1
				idbputs("add statement reference")
			end
		end
		return exprs,conds,nums,fcalls
	end

end
