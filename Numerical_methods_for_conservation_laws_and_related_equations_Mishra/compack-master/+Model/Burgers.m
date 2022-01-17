classdef Burgers < Model.ModelBase
	%BURGERS   Burgers' equation
	
	properties (SetAccess = private)
		name = 'Burgers'
		nelem = 1
	end
	
	
	methods
		function ret = f(obj, U, d)
			ret = U.*U / 2;
		end
	end
	
	
	methods(Access=protected)
		function ret = maxEigRect(obj, U, d)
			ret = abs(U);
		end
	end
end