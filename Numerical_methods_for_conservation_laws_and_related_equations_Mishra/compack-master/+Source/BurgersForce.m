classdef BurgersForce < Source.SourceBase
%BURGERSFORCE   Source term to force the solution to a specific function.
%   If the initial data is chosen to be a specific function (see source())
%   then this source term will force the (exact) solution to stay constant
%   in time.
	
	properties(Access = private)
		mesh
	end
	
	properties
	end
	
	
	methods
		function initialize(o, config)
			o.mesh = config.mesh;
		end
		
		
		function ret = source(o, U, UR, t, dt)
			if o.mesh.ndims == 1
				x = o.mesh.x;
			else
				x = o.mesh.x{1};
				y = o.mesh.x{2};
			end
			
			ret = zeros(size(U));
			
% 			% The function (x,y) -> exp(-k(x^2+y^2))
% 			k = 1;
% 			ret(o.mesh.internal) = -2*k*(x+y).*exp(-2*k*(x.^2+y.^2));
% % 			for i = 1:size(ret,1)
% % 				ret(i,o.mesh.internal) = -2*o.k*(x+y).*exp(-2*o.k*(x.^2+y.^2));
% % 			end

			% The function  -> |x|^(-1/3)
			ret(o.mesh.internal) = -1./(3*x.*abs(x).^(2/3));
		end
	end
end