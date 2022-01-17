classdef Exact_2nd < Flux.NumFlux
	%EXACT_2ND   Exact solver for second-order approximations for linear
	%            advection equation
	%   This method propagates the solution exactly when using second-order
	%   reconstruction.
	
	properties
		name = 'Exact_2nd'
	end
	
	methods
		function ret = F(o, d, Ul, Ur, t, dt, UlA, UrA)
			a = o.model.a(d);
			dx = o.mesh.dx{d};
			if a > 0
				ret = a*(Ul - a*dt./dx.*(Ul-UlA));
			else
				ret = a*Ur;
			end
		end
	end	
end