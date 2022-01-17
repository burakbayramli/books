classdef LinAdv < Model.ModelBase
	%LINADV   Linear advection
	
	properties (SetAccess = private)
		name = 'LinAdv'
		nelem = 1
	end
	
	properties
		a = [1,1]   % Advection speed in x- and y-directions
	end
	
	
	methods
		function ret = f(obj, U, d)
			ret = obj.a(d)*U;
		end
	end
	
	
	methods(Access=protected)
		function ret = maxEigRect(obj, U, d)
			ret = abs(obj.a(d)) * ones(size(U));
		end
	end
end