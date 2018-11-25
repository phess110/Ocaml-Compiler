class Arrayname
	include Comparable
	attr_accessor :name, :length
	def initialize(name="",length=0)
		unless name.is_a?(String) && length.is_a?(Integer)
			raise TypeError,"Invalid arguments to Arrayname.\nname must be a String and length must be an Integer"
		end
		@name=name
		@length=length
	end
	def <=>(other)
		if other.is_a?(Arrayname)
			[@name<=>other.name,@length<=>other.length]
		end
		@name<=>other
	end
	def to_s
		@name
	end
	def to_str
		to_s
	end
	def to_i
		@length
	end
	def to_int
		to_i
	end
	def ==(other)
		if other.is_a?(Arrayname)
			return @name==other.name && @length==other.length
		end
		@name==other
	end
	def eql?(other)
		@name.eql?(other)
	end
	def hash
		@name.hash
	end
end
