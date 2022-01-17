classdef EngOsh < Flux.NumFlux
	%ENGOSH   Engquist-Osher flux for Burgers' equation
	
	properties
		name = 'Engquist-Osher'
	end
	
	methods
		function ret = F(obj, d, Ul, Ur, varargin)
			ret = obj.f(max(Ul, 0), d) + obj.f(min(Ur, 0), d);
		end
	end
end