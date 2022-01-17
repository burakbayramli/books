classdef Central
%CENTRAL   Central slope
		
	methods (Static)
		function ret = slope(ul, um, ur)
			ret = (ur - ul)/2;
		end
	end
	
end

