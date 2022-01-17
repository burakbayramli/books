classdef Lim_SB
	%LIM_SB   Superbee limiter
		
	methods (Static)
		function ret = slope(ul, um, ur)
			ret = Utility.maxmod(...
				Utility.minmod(2*(um-ul), (ur-um)), ...
				Utility.minmod((um-ul), 2*(ur-um)));
		end
	end	
end

