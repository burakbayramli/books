classdef Godunov < Flux.NumFlux
	%GODUNOV   Godunov flux for Burgers' equation
	
	properties
		name = 'Godunov'
	end
	
	methods
		function ret = F(obj, d, Ul, Ur, varargin)
			ret = max(obj.f(max(Ul, 0),d), obj.f(min(Ur, 0),d));
		end
	end
end