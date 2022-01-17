classdef LinSys < Model.ModelBase
	
	properties (SetAccess = private)
		name = 'LinSys'
		nelem = 2
	end
	
	properties
		c = 1
	end
	
	
	methods
		function ret = f(obj, U, d)
			u1 = U(1,:,:);
			u2 = U(2,:,:);
			ret = [u1+u2; u1-u2];
		end
		
		
		function ret = maxEig(obj, U, d)
			ret = sqrt(2);
		end
	end
end