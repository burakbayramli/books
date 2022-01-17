classdef Lim_MM
	%Lim_MM   Minmod limiter
		
	methods (Static)
		function ret = slope(ul, um, ur)
			ret = Utility.minmod(um-ul, ur-um);
		end
	end
	
end

