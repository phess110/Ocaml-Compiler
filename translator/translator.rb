#!/usr/bin/ruby

require_relative 'parser'
require_relative 'node'
require_relative 'num'

class Transpiler < Parser

	attr_accessor :indent, :labelno

	def initialize(ifile,indent="\t")
		super(ifile)
		@indent=indent
		@labelno=0
	end

	def idputs(thing)
		if debug
			puts(thing)
		end
	end
	def idprint(thing)
		if debug
			print(thing)
		end
	end

	def translate(ofile=STDOUT)
		root=@root
		ddd=0
		root.nodes.each do |n|
			if n.type=="Preprocessor directive (Meta statement)"
				ofile<<n.str
				ofile<<"\n"
				ddd+=1
			end
		end
		dd=root.nodes[ddd]
		dd.nodes.each do |n|
			if n.type=="Preprocessor directive (Meta statement)"
				ofile<<n.str
				ofile<<"\n"
			end
		end
		gvn=[]
		gvnm=Hash.new
		if @numglob!=0
			ofile<<"int global[%d];\n"%root.misc["Global Vars"]
			gvn=root.get("Global Var Names")
			nglob=root.get("Global Vars")
			nglob.times do |i|
				gvnm.store(gvn[i],i)
			end
		end
		globarrs=root.get("Array Names")
		if globarrs==nil
			globarrs=[]
		end
		curr=root[ddd+1]
		until curr.str=="EPSILON"
			curr.nodes.each do |nn|
				if nn.type=="Preprocessor directive (Meta statement)"
					ofile<<nn.str
					ofile<<"\n"
				end
			end
			nxt=0
			while curr[nxt].type=="Preprocessor directive (Meta statement)"
				nxt+=1
			end
			idputs("doing function %s" % collate_id(curr[nxt][0][1],false))
			dofunc(curr[nxt],ofile,gvnm,globarrs)
			curr=curr[nxt+1]
		end
		if debug
			idputs("Done")
		end
		return
	end

	def doparamlist(node,ofile)
		if node.type!="Parameter_list"
			raise "Error while translating a parameter list.\nNode's type is %s" % node.type
		end
		if node.str=="EPSILON"
			return ""
		elsif node.str=="void"
			return "void"
		end
		nel=node[0]
		ofile<<nel[0].str
		ofile<<" "
		ofile<<collate_id(nel[1],false)
		nelt=nel[2]
		until nelt.str=="EPSILON"
			ofile<<", "
			ofile<<nelt[0].str
			ofile<<" "
			ofile<<collate_id(nelt[1],false)
			nelt=nelt[2]
		end
		return
	end
	
	def doexpression(node,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs=[],locarrs=[])
		if node.type!="Expression"
			raise "Error while translating an expression.\nNode's type is %s" % node.type
		end
		lastused=doterm(node[0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
		curr=node[1]#expr_tail
		while curr.str!="EPSILON"
			otmp=tempstrt
			idputs("calling term with node: %s" % curr[1])
			lu2=doterm(curr[1],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			ofile<<indent
			ofile<<"local[%d] = " % tempstrt
			tempmap.store("Addop for temp %d"%tempstrt,tempstrt)
			tempstrt.add(1)
			numtemps.sub(1)
			ofile<<"%s %s %s;\n" % [regnumstr(lastused),curr[0].str,regnumstr(lu2)]
			lastused=tempstrt.to_i-1
			idputs("take expr tail")
			curr=curr[2]
		end
		return lastused
	end

	def dofunc(node,ofile,globs=nil,globarrs=[])
		if node.type!="Func"
			raise "Error while translating a function.\nNode's type is %s" % node.type
		end
		fd=node[0]
		ft=node[1]
		ofile<<fd[0].str
		ofile<<" "
		ofile<<collate_id(fd[1],false)
		ofile<<"("
		doparamlist(fd[2],ofile)
		ofile<<")"#fd done
		if ft.str==";"
			ofile<<ft.str
			ofile<<"\n"
			return
		end
		ofile<<"\n{\n"
		if ft.get("Local Regs")!=0
			ofile<<indent
			ofile<<"int local[%d];\n" % ft.get("Local Regs")
		end
		lvmap=Hash.new
		base=0
		idprint(collate_id(fd[1],false))
		idputs(" num params: %d" % ft.get("Local Params").to_i)
		while base<ft.get("Local Params").to_i
			idputs("baseloop. base is %d" % base)
			lvmap.store(fd.get("Param Names")[base],base)
			ofile<<indent
			ofile<<"local[%d]=%s;\n" % [lvmap[fd.get("Param Names")[base]],fd.get("Param Names")[base]]
			base+=1
		end
		nbase=0
		while nbase<ft.get("Local Vars")
			idputs("nbase loop. nbase is %d. base is %d" % [nbase,base])
			lvmap.store(ft.get("Local Var Names")[nbase],nbase+base)
			nbase+=1
		end
		idputs("Printing variables and var map")
		idputs(ft.get("Local Var Names"))
		idputs(lvmap)
		temps=nbase+base#now for statements
		stats=ft[1]
		tempstrt=Num.new(lvmap.length)
		numtemps=Num.new(ft.get("Local Regs")-tempstrt)
		tempmap=Hash.new
		locarrs=ft.get("Array Names")
		if locarrs==nil
			locarrs=[]
		end
		idprint("Starting statements.\nlocal map: ")
		idputs(lvmap)
		until stats.str=="EPSILON"
			dostatement(stats[0],ofile,globs,lvmap,ft.get("Local Regs"),tempstrt,numtemps,tempmap,-1,globarrs,locarrs)
			stats=stats[1]
		end
		ofile<<"}\n\n"
	end

	def doterm(node,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs=[],locarrs=[])
		if node.type!="Term"
			raise "Error while translating a term.\nNode's type is %s" % node.type
		end
		idputs("calling factor with node %s" % node[0])
		lastused=dofactor(node[0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
		curr=node[1]#term_tail
		while curr.str!="EPSILON"
			otmp=tempstrt
			lu2=dofactor(curr[1],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			ofile<<indent
			ofile<<"local[%d] = " % tempstrt
			tempmap.store("Mulop for temp %d"%tempstrt,tempstrt)
			tempstrt.add(1)
			numtemps.sub(1)
			ofile<<"%s %s %s;\n" % [regnumstr(lastused),curr[0].str,regnumstr(lu2)]
			lastused=tempstrt.to_i-1
			idputs("take term tail")
			curr=curr[2]
		end
		return lastused
	end

	def regnumstr(n)
		if n.is_a?(Complex)
			if n.imag>0
				return "global[ local[%d] ]"%n.real
			else
				return "local[ local[%d] ]"%n.real
			end
		end
		if n<0
			n*=-1
			n-=100
			return "global[%d]"%n
		end
		return "local[%d]"%n
	end

	def collate_num(nd,prefix="")
		if nd.type!="Number"
			raise "Error collating a number.\nPassed node type is %s" % nd.type
		end
		str=prefix
		str<<nd[0].str
		curr=nd[1]
		until curr.str=="EPSILON"
			str<<curr[0].str
			curr=curr[1]
		end
		return str
	end

	def dofactor(node,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs=[],locarrs=[])
		if node.type!="Factor"
			raise "Error while translating a factor.\nNode's type is %s" % node.type
		end
		if node[0].type=="Number"#positive or negative number
			nstr=""
			if node.str=="-"
				nstr=collate_num(node[0],"-")
			else
				nstr=collate_num(node[0])
			end
			ofile<<indent
			ofile<<"local[%d] = %s;\n" % [tempstrt,nstr]
			tempmap.store("Factor, the number %s" % nstr,tempstrt)
			tempstrt.add(1)
			idputs("take factor number %s" % nstr)
			numtemps.sub(1)
			return tempstrt-1
		elsif node[0].type=="Expression"#parenthesized expression
			lu=doexpression(node[0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			return lu
		else#identifier factor_tail
			reg=""
			rnum=-1
			id=collate_id(node[0],false)
			if globmap[collate_id(node[0],false)]!=nil
				rnum=globmap[collate_id(node[0],false)]
				rnum+=100
				rnum*=-1
				return rnum
			elsif globarrs.include?(id)
				idnode=node[0]
				if idnode[1].str=="EPSILON"
					raise "Error: Pointer arithmetic for array access is not allowed."
				end
				lu=doexpression(idnode[1][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
				ofile<<indent<<"local[%d] = %d + %s;\n" % [tempstrt,globmap["&%s+0" % id],regnumstr(lu)]
				rnum=tempstrt.to_i
				tempstrt.add(1)
				idputs("take factor array tail global")
				numtemps.sub(1)
				return rnum+1i
			elsif locarrs.include?(id)
				idnode=node[0]
				if idnode[1].str=="EPSILON"
					raise "Error: Pointer arithmetic for array access is not allowed."
				end
				lu=doexpression(idnode[1][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
				ofile<<indent<<"local[%d] = %d + %s;\n" % [tempstrt,locmap["&%s+0" % id],regnumstr(lu)]
				rnum=tempstrt.to_i
				tempstrt.add(1)
				idputs("take factor array tail local")
				numtemps.sub(1)
				return rnum-1i
			elsif locmap[collate_id(node[0],false)]!=nil
				rnum=locmap[collate_id(node[0],false)]
				return rnum
			end#function call
			regs=[]
			regs=doexprlist(node[1][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			ofile<<indent
			ofile<<"local[%d] = " % tempstrt
			ofile<<collate_id(node[0],false)
			ofile<<"("
			until regs.empty?
				ofile<<"%s%s" % [regnumstr(regs.shift),regs.empty?() ? "" : ", "]
			end
			ofile<<");\n"
			tempmap.store("Factor, function call in local %d",tempstrt)
			tempstrt.add(1)
			idputs("take factor func call")
			numtemps.sub(1)
			return tempstrt-1
		end
	end

	def doexprlist(node,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs=[],locarrs=[])
		if node.type!="Expr_list"
			raise "Error while translating an expression list.\nNode's type is %s" % node.type
		end
		if node.str=="EPSILON"
			return []
		end
		out=[]
		out<<doexpression(node[0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
		curr=node[1]
		until curr.str=="EPSILON"
			out<<doexpression(curr[0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			curr=curr[1]
		end
		return out
	end

	def doblockstatements(node,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,labelno=-1,globarrs=[],locarrs=[])
		if node.type!="Block_statements"
			raise "Error while translating a block of statements.\nNode's type is %s" % node.type
		end
		stats=node[0]
		until stats.str=="EPSILON"
			dostatement(stats[0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,labelno,globarrs,locarrs)
			stats=stats[1]
		end
	end

	def docondition(node,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs=[],locarrs=[])
		if node.type!="Condition"
			raise "Error while translating a condition.\nNode's type is %s" % node.type
		end
		lu1=doexpression(node[0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
		lu2=doexpression(node[2],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
		ofile<<indent<<"local[%d] = %s %s %s;\n" % [tempstrt,regnumstr(lu1),node[1].str,regnumstr(lu2)]
		lastused=tempstrt.to_i
		tempmap.store("Condition for temp %d" % tempstrt,tempstrt)
		tempstrt.add(1)
		idputs("take condition")
		numtemps.sub(1)
		return lastused
	end

	def docondexpr(node,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs=[],locarrs=[])
		if node.type!="Condition_expression"
			raise "Error while translating a condition expression.\nNode's type is %s" % node.type
		end
		idputs("calling condition with node %s" % node[0])
		lastused=docondition(node[0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
		curr=node[1]#cond_exp_tail
		unless curr.str=="EPSILON"
			lu2=docondition(curr[1],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			ofile<<indent<<"local[%d] = local[%d] %s local[%d];\n" % [tempstrt,lastused,curr[0].str,lu2]
			lastused=tempstrt.to_i
			tempstrt.add(1)
			idputs("take cond expr tail")
			numtemps.sub(1)
		end
		return lastused
	end

	def dostatement(node,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,labelno=-1,globarrs=[],locarrs=[])
		if node.type!="Statement"
			raise "Error while translating a statement.\nNode's type is %s" % node.type
		end
		if node[0].type=="Identifier"#assig or call
			if node[1][0].type=="Expression"#assignment
				lu=doexpression(node[1][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
				id=collate_id(node[0],false)
				if globmap[id]!=nil#assign a global
					ofile<<indent
					ofile<<"global[%d] = %s;\n" % [globmap[id],regnumstr(lu)]
				elsif globarrs.include?(id)
					idnode=node[0]
					if idnode[1].str=="EPSILON"
						raise "Error: Pointer arithmetic for array access is not allowed."
					end
					lu2=doexpression(idnode[1][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
					ofile<<indent<<"local[%d] = %d + %s;\n" % [tempstrt,globmap["&%s+0" % id],regnumstr(lu2)]
					ofile<<indent<<"global[ local[%d] ] = %s;\n" % [tempstrt,regnumstr(lu)]
					tempstrt.add(1)
					idputs("take global statement ref")
					numtemps.sub(1)
				elsif locarrs.include?(id)
					idnode=node[0]
					if idnode[1].str=="EPSILON"
						raise "Error: Pointer arithmetic for array access is not allowed."
					end
					lu2=doexpression(idnode[1][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
					ofile<<indent<<"local[%d] = %d + %s;\n" % [tempstrt,locmap["&%s+0" % id],regnumstr(lu2)]
					ofile<<indent<<"local[ local[%d] ] = %s;\n" % [tempstrt,regnumstr(lu)]
					tempstrt.add(1)
					idputs("take local statement ref")
					numtemps.sub(1)
				else#assign a local
					ofile<<indent
					ofile<<"local[%d] = %s;\n" % [locmap[id],regnumstr(lu)]
				end
			else#call
				id=collate_id(node[0],false)
				regs=[]
				regs=doexprlist(node[1][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
				ofile<<indent<<id<<"("
				until regs.empty?
					ofile<<"%s%s" % [regnumstr(regs.shift),regs.empty?() ? "" : ", "]
				end
				ofile<<");\n"
			end
		elsif node[0].type=="Printf_func_call"
			lu=-1
			if node[0][1].str!="EPSILON"
				lu=doexpression(node[0][1][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			end
			ofile<<indent
			ofile<<"printf("
			ofile<<node[0][0].str
			if lu!=-1
				ofile<<", "
				ofile<<"%s"%regnumstr(lu)
			end
			ofile<<");\n"
		elsif node[0].type=="Scanf_func_call"
			curr=node[0]
			lu=doexpression(curr[1],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			ofile<<indent
			ofile<<"scanf(%s, &%s);\n" % [curr[0],regnumstr(lu)]
		elsif node[0].type=="Return_statement"
			lu=-1
			curr=node[0]
			if curr[0].str!="EPSILON"
				lu=doexpression(curr[0][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			end
			ofile<<indent<<"return"
			if lu!=-1
				ofile<<" %s"%regnumstr(lu)
			end
			ofile<<";\n"
		elsif node[0].type=="Break_statement"
			raise "Error: break outside of breakable block" unless labelno!=-1
			ofile<<indent<<"goto LABEL%dOUT;\n" % labelno
		elsif node[0].type=="Continue_statement"
			raise "Error: continue outside of loop" unless lebalno!=-1
			ofile<<indent<<"goto LABEL%dSTART;\n" % labelno
		elsif node[0].type=="While_statement"
			thislabel=@labelno
			@labelno+=1
			ofile<<indent<<"LABEL%dSTART:;\n"%thislabel#start of loop. structure: start: if(x==0) goto out; <statements>; goto start; out: <end of structure>
			lu=docondexpr(node[0][0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			ofile<<indent<<"if(local[%d] == 0) goto LABEL%dOUT;\n" % [lu,thislabel]
			doblockstatements(node[0][1],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,thislabel,globarrs,locarrs)
			ofile<<indent<<"goto LABEL%dSTART;\n" % thislabel
			ofile<<indent<<"LABEL%dOUT:;\n" % thislabel
		else#if statement, structure: if(x!=0) goto doit; <else parts>; goto out; doit: <then parts>; out: <end of structure>, different structure for no else: if(x==0) goto out; <then parts>; out:<end>
			is=node[0]
			cond=is[0]
			blo=is[1]
			els=is[2]
			thislabel=@labelno
			@labelno+=1
			lu=docondexpr(cond,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,globarrs,locarrs)
			ofile<<indent<<"if(local[%d]"%lu
			if els.str=="EPSILON"#no else statement
				ofile<<" == 0) goto LABEL%dOUT;\n" % thislabel
				doblockstatements(blo,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,labelno,globarrs,locarrs)
				ofile<<indent<<"LABEL%dOUT:;\n" % thislabel
			else#there is an else
				ofile<<" != 0) goto LABEL%dTHEN;\n" % thislabel
				doblockstatements(els[0],ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,labelno,globarrs,locarrs)
				ofile<<indent<<"goto LABEL%dOUT;\n" % thislabel
				ofile<<indent<<"LABEL%dTHEN:;\n" % thislabel
				doblockstatements(blo,ofile,globmap,locmap,numlocals,tempstrt,numtemps,tempmap,labelno,globarrs,locarrs)
				ofile<<indent<<"LABEL%dOUT:;\n" % thislabel
			end
		end
		idputs("End of stetement. Printing a lot of stuff")
		idprint("globmap: ")
		idputs(globmap)
		idprint("locmap: ")
		idputs(locmap)
		idprint("tempmap: ")
		idputs(tempmap)
		idputs("tempstrt: %d" % tempstrt)
		idputs("numtemps: %d" % numtemps)
	end

end

inf=ARGV[0]
pa=Transpiler.new(inf)
pa.debug=false
doroot=false
if ARGV.length==3 &&  ARGV[2]=="print"
	doroot=true
elsif ARGV.length==3 && ARGV[2]=="debug"
	pa.debug=true
	doroot=true
end
pa.start
pa.idputs("Finished parsing")
if doroot
	puts("Printing parse tree")
	pa.root.pretty(0)
end
if ARGV.length==1 || (ARGV.length>1 && ARGV[1]=="-")
	pa.translate
else
	ofile=File.open(ARGV[1],"w")
	pa.translate(ofile)
end

