classdef EC < Flux.NumFlux
%EC   Entropy conservative (EC) flux for the shallow water equations
	
	properties
		name = 'EC'
	end
	
	
	methods
		function ret = F(o, d, Ul, Ur, varargin)
			grav = o.model.grav;
			
			hl = Ul(1,:,:);
			hr = Ur(1,:,:);
			% Velocities
			ul = Ul(2,:,:) ./ hl;
			ur = Ur(2,:,:) ./ hr;
			vl = Ul(3,:,:) ./ hl;
			vr = Ur(3,:,:) ./ hr;
			
			hAvg = (hl+hr)/2;
			hSqAvg = (hl.^2 + hr.^2)/2;
			uAvg = (ul + ur)/2;
			vAvg = (vl + vr)/2;
			
			if d == 1
				ret = [hAvg.*uAvg;
					hAvg.*uAvg.^2 + 0.5*grav*hSqAvg;
					hAvg.*uAvg.*vAvg];
			else
				ret = [hAvg.*vAvg;
					hAvg.*uAvg.*vAvg;
					hAvg.*vAvg.^2 + 0.5*grav*hSqAvg];
			end
		end
	end
	
	
% 	methods (Access=private)
% 		function ret = Fx(o, Ul, Ur)
% 			grav = o.model.grav;
% 			
% 			hl = Ul(1,:,:);
% 			hr = Ur(1,:,:);
% 			% Velocities
% 			ul = Ul(2,:,:) ./ hl;
% 			ur = Ur(2,:,:) ./ hr;
% 			vl = Ul(3,:,:) ./ hl;
% 			vr = Ur(3,:,:) ./ hr;
% 			
% 			hAvg = (hl+hr)/2;
% 			hSqAvg = (hl.^2 + hr.^2)/2;
% 			uAvg = (ul + ur)/2;
% 			vAvg = (vl + vr)/2;
% 			
% 			ret = [hAvg.*uAvg;
% 				hAvg.*uAvg.^2 + 0.5*grav*hSqAvg;
% 				hAvg.*uAvg.*vAvg];
% 		end
% 		
% 		
% 		function ret = Fy(o, Ul, Ur)
% 			grav = o.model.grav;
% 			
% 			hl = Ul(1,:,:);
% 			hr = Ur(1,:,:);
% 			% Velocities
% 			ul = Ul(2,:,:) ./ hl;
% 			ur = Ur(2,:,:) ./ hr;
% 			vl = Ul(3,:,:) ./ hl;
% 			vr = Ur(3,:,:) ./ hr;
% 			
% 			hAvg = (hl+hr)/2;
% 			hSqAvg = (hl.^2 + hr.^2)/2;
% 			uAvg = (ul + ur)/2;
% 			vAvg = (vl + vr)/2;
% 			
% 			ret = [hAvg.*vAvg;
% 				hAvg.*uAvg.*vAvg;
% 				hAvg.*vAvg.^2 + 0.5*grav*hSqAvg];
% 		end
% 	end
end