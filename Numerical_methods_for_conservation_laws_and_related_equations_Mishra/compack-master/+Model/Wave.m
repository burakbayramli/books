classdef Wave < Model.ModelBase
	%WAVE   Wave equation
	
	properties (SetAccess = private)
		name = 'Wave'
		nelem = 3
	end
	
	properties
		c = 1
	end
	
	properties (Access = private)
		R
		RInv
	end
	
	
	methods
		function obj = Wave
			% Precompute the eigenvector matrices
			s = 1/sqrt(2);
			obj.R = [s, s, 0; s, -s, 0; 0, 0, 1];
			obj.RInv = obj.R;
		end
		
				
		function ret = f(obj, U, dir)
			ret = zeros(size(U));
			ret(1,:) = obj.c * U(dir+1, :);
			ret(dir+1,:) = obj.c * U(1,:);
		end
		
		
		%EIGENVECTORS
		% Compute the eigenvectors of the derivative of the flux function
		% in direction 'd', evaluated at a point U.
		function [R, RInv] = eigenvectors(obj, U, d)
			R = obj.R;
			RInv = obj.RInv;
		end
		
		
		%EIGENVALUES
		% Returns a diagonal matrix of eigenvalues of the derivative of the
		% flux function in direction 'd', evaluated at a point U.
		function Gamma = eigenvalues(obj, U, d)
			Gamma = diag([-obj.c, 0, obj.c]);
		end
	end
	
	
	methods (Access=protected)
		function ret = maxEigRect(obj, U, d)
			ret = obj.c;
		end
		
		
		function ret = doGetVariable(o, soln, U, varname )
			if strcmpi(varname, 'm1') || strcmpi(varname, 'u1')
				ret = U(2,:,:);
			elseif strcmpi(varname, 'm2') || strcmpi(varname, 'u2')
				ret = U(3,:,:);
			end
		end
	end
end