classdef Backward
%BACKWARD   Backwards slope
		
	methods (Static)
		function ret = slope(ul, um, ur)
			ret = um - ul;
		end
	end
	
end

