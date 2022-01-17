classdef Lim_MC
	%LIM_MC   MC limiter
		
	methods (Static)
		function ret = slope(ul, um, ur)
			ret = Utility.minmod(2*(um-ul), 2*(ur-um), (ur-ul)/2);
		end
	end
	
end

