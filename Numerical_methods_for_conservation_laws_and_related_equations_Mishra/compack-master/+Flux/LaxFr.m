classdef LaxFr < Flux.NumFlux
	%LAXFR   Lax-Friedrichs numerical flux
	
	
	properties
		name = 'Lax-Friedrichs'
	end
	
	methods
		function ret = F(o, d, Ul, Ur, t, dt, varargin)
			dx = o.mesh.dx{d};
			ret = 0.5*(o.f(Ul,d) + o.f(Ur,d)) - 0.5/o.mesh.ndims*(dx/dt).*(Ur-Ul);
		end
	end
end