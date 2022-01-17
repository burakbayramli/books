classdef Rusanov < Flux.NumFlux
	%RUSANOV   Rusanov numerical flux.
	
	properties
		name = 'Rusanov'
	end
	
	
	methods
		function ret = F(obj, dir, Ul, Ur, t, dt, varargin)
			% Find maximum eigenvalues
			maxEig = max(obj.maxEig(Ul, dir), obj.maxEig(Ur, dir));
			% Compute maxEig*(Ur-Ul). As maxEig may be a vector or array
			% (in nonlinear equations where the eigenvalues are functions
			% of U), this is done through the function timesArray().
			diffusion = Utility.timesArray(maxEig, Ur-Ul);
			% Compute the final numerical flux.
			ret = 0.5*(obj.f(Ul, dir) + obj.f(Ur, dir)) - 0.5*diffusion;
		end
	end
end