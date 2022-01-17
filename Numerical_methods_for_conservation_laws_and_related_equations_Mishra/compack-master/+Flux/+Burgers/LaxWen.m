classdef LaxWen < Flux.NumFlux
	%LAXWEN   Lax-Wendroff numerical flux for Burgers' equation.
	
	
	properties
		name = 'Lax-Wendroff'
	end
	
	methods
		function ret = F(obj, d, Ul, Ur, t, dt, varargin)
			dx = obj.mesh.dx{d};
			a = (Ul + Ur)/2;
			ret = 0.5*(obj.f(Ul, d) + obj.f(Ur, d)) ...
				- 0.5*a.*(dt./dx).*(obj.f(Ur, d) - obj.f(Ul, d));
		end
	end
end