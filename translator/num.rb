class Num < Numeric
	include Comparable
	attr_accessor :val
	def +(other)
		return @val+other	
	end
	def -(other)
		return @val-other
	end
	def -@
		return -@val
	end
	def initialize(v)
		@val=v
	end
	def to_s
		@val.to_s
	end
	def inspect
		@val.inspect
	end
	def to_i
		@val
	end
	def add(other)
		@val+=other
	end
	def sub(other)
		@val-=other
	end
	def <=>(other)
		@val<=>other
	end
	def *(other)
		@val*other
	end
	def /(other)
		@val/other
	end
	def coerce(other)
		[self,other]
	end
end
