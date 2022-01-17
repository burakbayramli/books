classdef Roe < Flux.NumFlux
	%ROE   Roe flux for Burgers' equation
	
	properties
		name = 'Roe'
	end
	
	methods
		function ret = F(obj, d, Ul, Ur, varargin)
			ret = (Ul >= -Ur).*obj.f(Ul, d) + (Ul < -Ur).*obj.f(Ur, d);
		end
	end
end