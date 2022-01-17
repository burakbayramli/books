classdef EC < Flux.NumFlux
%EC   Entropy conservative (EC) flux for the shallow water equations on
%	triangular meshes.
	
	properties
		name = 'EC'
	end
	
	
	methods
		function ret = F(o, n, Ul, Ur, varargin)
% 			f1 = o.Fx(Ul, Ur);
% 			f2 = o.Fy(Ul, Ur);
% 			ret = Utility.timesArray(n(1,:),f1) + Utility.timesArray(n(2,:),f2);

			grav = o.model.grav;
			
			hl = Ul(1,:);
			hr = Ur(1,:);
			% Velocities
			ul = Utility.divideArray(Ul([2,3],:), hl);
			ur = Utility.divideArray(Ur([2,3],:), hr);
			
			hAvg = (hl+hr)/2;
			hSqAvg = (hl.^2 + hr.^2)/2;
			uAvg = (ul + ur)/2;				% Average velocity
			sAvg = dot(uAvg, n, 1);	% Average speed in direction 'normal'
			hs = hAvg .* sAvg;
			
			ret = [hs;
				uAvg .* [hs; hs] + 0.5*grav*n.*[hSqAvg; hSqAvg]];
			
% 			maxEig = max([o.maxEig(Ul, 1), o.maxEig(Ur, 1), ...
% 						  o.maxEig(Ul, 2), o.maxEig(Ur, 2)]);
% 			diffusion = Utility.timesArray(maxEig, Ur-Ul);
% 			ret = ret - 0.5*diffusion;
		end
	end
	
	
	methods (Access = private)
		function ret = Fx(o, Ul, Ur)
			grav = o.model.grav;
			
			hl = Ul(1,:);
			hr = Ur(1,:);
			% Velocities
			ul = Ul(2,:) ./ hl;
			ur = Ur(2,:) ./ hr;
			vl = Ul(3,:) ./ hl;
			vr = Ur(3,:) ./ hr;
			
			hAvg = (hl+hr)/2;
			hSqAvg = (hl.^2 + hr.^2)/2;
			uAvg = (ul + ur)/2;
			vAvg = (vl + vr)/2;
			
			ret = [hAvg.*uAvg;
				hAvg.*uAvg.^2 + 0.5*grav*hSqAvg;
				hAvg.*uAvg.*vAvg];
		end
		
		
		function ret = Fy(o, Ul, Ur)
			grav = o.model.grav;
			
			hl = Ul(1,:);
			hr = Ur(1,:);
			% Velocities
			ul = Ul(2,:) ./ hl;
			ur = Ur(2,:) ./ hr;
			vl = Ul(3,:) ./ hl;
			vr = Ur(3,:) ./ hr;
			
			hAvg = (hl+hr)/2;
			hSqAvg = (hl.^2 + hr.^2)/2;
			uAvg = (ul + ur)/2;
			vAvg = (vl + vr)/2;
			
			ret = [hAvg.*vAvg;
				hAvg.*uAvg.*vAvg;
				hAvg.*vAvg.^2 + 0.5*grav*hSqAvg];
		end
	end
end