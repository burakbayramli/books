classdef Euler < Model.ModelBase
	%Euler   Euler equations
	
	properties (SetAccess = private)
		name = 'Euler'
		nelem = 4
	end
	
	properties
		gamma = 1.4		% Adiabatic exponent
	end
	
	
	methods
		function ret = f(o, U, d)
			rho = U(1,:);
			m = U(2:3,:);
			E = U(4,:);
			p = (o.gamma - 1) * (E - 0.5*sum(m.^2) ./ rho);
			uD = m(d,:) ./ rho;	% Speed in d-direction
			
			ret = [m(d,:); 
				uD.*m(1,:); 
				uD.*m(2,:); 
				(E+p).*uD];
			ret(d+1,:) = ret(d+1,:) + p;
			ret = reshape(ret, size(U));
		end
				
		
		% Returns true if negative pressure or density is encountered
		function ret = breaksPositivity(o, U)
			rho = U(1,:);
			p = o.pressure(U);
			ret = any(rho < 0) || any(p(:) < 0);
		end
		
				
		function [rho, m1, m2, E] = getConservedVars(o, U)
			rho = U(1,:,:);
			m1 = U(2,:,:);
			m2 = U(3,:,:);
			E = U(4,:,:);
		end
		
				
		% primToCons()
		% Converts primitive variables rho, u, v, p to conserved variables
		% m1, m2, E.
		function [m1, m2, E] = primToCons(o, rho, u, v, p)
			m1 = rho.*u;
			m2 = rho.*v;
			E = p/(o.gamma-1) + 0.5*rho.*(u.*u+v.*v);
		end
		
				
		% pressure()
		% Returns the pressure p of the solution U
		function p = pressure(o, U)
			[rho, m1, m2, E] = getConservedVars(o, U);
			p = (o.gamma - 1) * (E - 0.5*(m1.*m1 + m2.*m2) ./ rho);
		end
	end
	
	
	methods(Access=protected)
		function ret = maxEigRect(o, U, d)
			rho = U(1,:,:);
			m = U(d+1,:,:);
			u = m ./ rho;
			p = o.pressure(U);
			c = sqrt(o.gamma * p./rho);
			ret = abs(u) + c;
		end
	end
	
	
	methods (Access=protected)
		function ret = doGetVariable(o, soln, U, varname)
			function ret = isvar(str)
				ret = strcmpi(varname, str);
			end

			if isvar('density') || isvar('rho') || isvar('')
				ret = U(1, :, :);
			elseif isvar('m1')
				ret = U(2, :, :);
			elseif isvar('u')
				ret = U(2,:,:) ./ U(1,:,:);
			elseif isvar('m2')
				ret = U(3, :, :);
			elseif isvar('v')
				ret = U(3,:,:) ./ U(1,:,:);
			elseif isvar('E')
				ret = U(4, :, :);
			elseif isvar('p') || isvar('pressure')
				rho = U(1,:,:);   m1 = U(2,:,:);   m2 = U(3,:,:);
				ret = (o.gamma-1)*(U(4,:,:) - 0.5*(m1.*m1 + m2.*m2) ./ rho);			
			elseif isvar('entropy')
				p = o.getVariable(soln,U,'p');   rho = U(1,:,:);
				ret = -rho .* log(p ./ pow(rho, o.gamma));	% TODO: Confirm this
			end
		end
	end
end
