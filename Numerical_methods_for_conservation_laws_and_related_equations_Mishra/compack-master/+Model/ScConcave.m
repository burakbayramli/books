classdef ScConcave < Model.ModelBase
	
	properties (SetAccess = private)
		name = 'ScConcave'
		nelem = 1
	end
	
	
	methods
		function ret = f(o, U, dir)
			ret = U.*(1-U)/2;
		end
		
		
		function ret = maxEig(o, U, d)
			ret = abs(U-0.5);
		end
	end
end