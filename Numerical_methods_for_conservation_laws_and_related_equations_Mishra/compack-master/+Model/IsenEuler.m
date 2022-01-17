classdef IsenEuler < Model.ModelBase

	properties (SetAccess = private)
		name = 'IsenEuler'
		nelem = 4
	end
	
	properties
		gamma = 1.4		% Adiabatic exponent
	end
	
	
	methods
		function ret = f(o, U, d)
			rho = U(1,:);
			m = U(2:3,:);
			q = U(4,:);
			p = rho.^o.gamma;
			uD = m(d,:) ./ rho;	% Speed in d-direction
			
			ret = [m(d,:); 
				uD.*m(1,:); 
				uD.*m(2,:); 
				uD.*q];
			ret(d+1,:) = ret(d+1,:) + p;
			ret = reshape(ret, size(U));
		end
				
		
		% Returns true if negative density is encountered
		function ret = breaksPositivity(o, U)
			rho = U(1,:);
			ret = any(rho < 0);
		end
		
				
		function [rho, m1, m2, q] = getConservedVars(o, U)
			rho = U(1,:,:);
			m1 = U(2,:,:);
			m2 = U(3,:,:);
			q = U(4,:,:);
		end
		
				
		% primToCons()
		% Converts primitive variables rho, u, v, p to conserved variables
		% m1, m2, E.
		function [m1, m2, q] = primToCons(o, rho, u, v, s)
			m1 = rho.*u;
			m2 = rho.*v;
			q = rho.*s;
		end
		
				
		% pressure()
		% Returns the pressure p of the solution U
		function p = pressure(o, U)
			rho = U(1,:,:);
			p = rho.^o.gamma;
		end
	end
	
	
	methods(Access=protected)
		function ret = maxEigRect(o, U, d)
			rho = U(1,:,:);
			m = U(d+1,:,:);
			u = m ./ rho;
			c = sqrt(o.gamma * rho.^(o.gamma-1));
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
			elseif isvar('q')
				ret = U(4,:,:);
			elseif isvar('p') || isvar('pressure')
				ret = o.pressure(U);			
% 			elseif isvar('energy')
% 				p = o.pressure(U);   rho = U(1,:,:);
% 				ret = -rho .* log(p ./ pow(rho, o.gamma));	% TODO: Confirm this
			end
		end
	end
end