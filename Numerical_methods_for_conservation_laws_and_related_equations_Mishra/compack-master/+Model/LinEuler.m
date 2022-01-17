classdef LinEuler < Model.ModelBase
	%LINEULER   Linearized Euler equations
	
	properties (SetAccess = private)
		name = 'LinEuler'
		nelem = 4
	end
	
	properties
		gamma = 1.4
		rho0 = 1
		u0 = [0, 0];	% Background velocity (speed in x- and y-direction)
		p0 = 1
	end
	
	properties (Dependent = true)
		c0
	end
	
	
	methods
		function ret = f(obj, U, d)
			rho = U(1,:);
			u = U(d+1,:);
			u2 = U(4-d,:);
			p = U(4,:);
			ret = zeros(size(U));
			ret(1,:) = obj.u0(d)*rho + obj.rho0*u;
			ret(d+1,:) = obj.u0(d)*u + p/obj.rho0;
			ret(4-d,:) = obj.u0(d)*u2;
			ret(4,:) = obj.p0*obj.gamma*u + obj.u0(d)*p;
		end
		
		
		function c = get.c0(obj)
			c = sqrt(obj.p0*obj.gamma / obj.rho0);
		end
		
		
		%EIGENVECTORS
		% Compute the eigenvectors of the derivative of the flux function
		% in direction 'd', evaluated at a point U.
		function [R, RInv] = eigenvectors(obj, U, d)
			c = obj.c0;
			rho = obj.rho0;
			R = [1/(c*c), 1/(c*c), 1, 0;
				-1/(rho*c), 1/(rho*c), 0, 0;
				0, 0, 0, 1;
				1, 1, 0, 0];
			RInv = [0, -c*rho/2, 0, 0.5;
				0, c*rho/2, 0, 0.5;
				1, 0, 0, -1/(c*c);
				0, 0, 1, 0];
		end
		
		
		%EIGENVALUES
		% Returns a diagonal matrix of eigenvalues of the derivative of the
		% flux function in direction 'd', evaluated at a point U.
		function Gamma = eigenvalues(obj, U, d)
			c = obj.c0;
			u = obj.u0(d);
			Gamma = diag(abs([u-c, u+c, u, u]));
		end
	end
	
	
	methods (Access=protected)
		function ret = maxEigRect(obj, U, d)
			ret = abs(obj.u0(d)) + obj.c0;
		end
		
		
		function ret = doGetVariable(o, soln, U, varname )
			function ret = isvar(str)
				ret = strcmpi(varname, str);
			end
			
			if isvar('rho')
				ret = U(1, :, :);
			elseif isvar('u')
				ret = U(2, :, :);
			elseif isvar('v')
				ret = U(3,:,:);
			elseif isvar('p') || isvar('pressure')
				ret = U(4, :, :);
			end
		end
	end
end