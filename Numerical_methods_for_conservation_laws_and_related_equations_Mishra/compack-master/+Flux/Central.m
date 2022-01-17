classdef Central < Flux.NumFlux
%CENTRAL   The central scheme
	
	
	properties
		name = 'Central'
	end
	
	methods
		function ret = F(o, d, Ul, Ur, t, dt, varargin)
			ret = (o.f(Ul,d) + o.f(Ur,d))/2;
		end
	end
end