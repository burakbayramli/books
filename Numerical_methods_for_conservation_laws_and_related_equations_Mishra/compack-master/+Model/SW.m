classdef SW < Model.ModelBase
	%SW   Shallow water system
	
	properties (SetAccess = private)
		name = 'SW'
		nelem = 3
	end
	
	properties
		grav = 1		% Gravitational constant
	end
	
	
	methods
		function ret = f(o, U, dir)
			dir2 = 4-dir;
			
			h = U(1,:);				% Water depth
			m = U(dir+1, :);		% Momentum in the direction 'dir'
			mOrth = U(dir2, :);		% Momentum in the direction orthogonal to 'dir'
			ret = zeros(size(U));
			ret(1,:) = m;
			ret(dir+1,:) = 0.5*o.grav*h.*h + m.*m./h;
			ret(dir2,:) = m.*mOrth./h;
		end
		
		
		% breaksPositivity()
		% Returns true if a negative height value is encountered in U.
		function ret = breaksPositivity(o, U)
			ret = any(U(1,:) < 0);
		end
	end
	
	
	methods (Access=protected)
		function ret = maxEigRect(o, U, d)
			h = U(1,:,:);
			u = U(d+1,:,:) ./ h;
			ret = abs(u) + sqrt(o.grav*h);
		end
		
		
		function ret = maxEigDir(o, U, n)
			h = U(1,:,:);
			u = dot(U(2:3,:,:), n) ./ h;
			ret = abs(u) + sqrt(o.grav*h);
		end
	end
	
	
	
	methods (Access=protected)
		function ret = doGetVariable(o, soln, U, varname)
			function ret = isvar(str)
				ret = strcmpi(varname, str);
			end
			
			if isvar('h')
				ret = U(1, :, :);
			elseif isvar('hu') || isvar('m1')
				ret = U(2, :, :);
			elseif isvar('hv') || isvar('m2')
				ret = U(3, :, :);
			elseif isvar('b')
				ret = soln.config.source.bEval(soln.mesh.internal);
			elseif isvar('h+b')
				% Return both b and h+b
				b = soln.config.source.bEval(soln.mesh.internal);
				ret = { U(1, :, :) + b, b};
			elseif isvar('energy')
				g = o.grav;
				h = U(1,:,:);
				q1 = U(2,:,:);
				q2 = U(3,:,:);
				ret = ((q1.*q1 + q2.*q2)./h + g*h.*h)/2;
			end	
		end
	end
end