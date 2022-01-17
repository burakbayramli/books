classdef Upwind < Flux.NumFlux
	%UPWIND   Upwind scheme for the linear advection equation
	
	properties
		name = 'Upwind'
	end
	
	methods
		function ret = F(o, d, Ul, Ur, varargin)
			a = o.model.a(d);
			if a > 0
				ret = a*Ul;
			else
				ret = a*Ur;
			end
		end
	end	
end