classdef Lim_MaxM
		
	methods (Static)
		function ret = slope(ul, um, ur)
			ret = Utility.maxmod(um-ul, ur-um);
		end
	end
	
end

