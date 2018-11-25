class Node
	attr_accessor :nodes, :type, :str, :num, :parent, :misc
	def to_s
		"Node: {Type: %s, String: %s, Child nodes: %p}" % [@type, @str, @nodes]
	end

	def initialize
		@nodes=[]
		@hbp=false
		@parent=nil
		@misc=Hash.new
	end

	def <<(nd)
		@nodes <<= nd
		@nodes[-1].parent=self
	end

	def >>(nd)
		@nodes.prepend(nd)
		@nodes[0].parent=self
	end

	def insert(ind,nd)
		@nodes.insert(ind,nd)
		@nodes[ind].parent=self
	end

	def [](ind)
		@nodes[ind]
	end

	def numkids
		return @nodes.length
	end

	def inspect
		to_s
	end

	def has(key)
		return @misc.has_key?(key)
	end

	def get(key)
		return @misc[key]
	end

	def pretty(tabs,str="\t")
		print(str*tabs)
		puts("%s%s" % [@type,@str==nil ?  "" :(": \"%s\"" % @str)])
		for kid in @nodes
			kid.pretty(tabs+1)
		end
	end
end


