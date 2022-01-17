classdef Forward
%FORWARD   Forwards slope
		
	methods (Static)
		function ret = slope(ul, um, ur)
			ret = ur - um;
		end
	end
	
end

