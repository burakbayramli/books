classdef Rusanov < Flux.NumFlux
%RUSANOV   Rusanov numerical flux on triangular meshes
	
	properties
		name = 'Rusanov'
	end
	
	
	methods
		% F()
		% Numerical flux
		function ret = F(o, n, Ul, Ur, varargin)
			% Find maximum eigenvalues
			maxEig = max([o.maxEig(Ul, n); o.maxEig(Ur, n)]);
			
			% Compute maxEig*(Ur-Ul). As maxEig may be a vector or array
			% (in nonlinear equations where the eigenvalues are functions
			% of U), this is done through the function timesArray().
			diffusion = 0.5*Utility.timesArray(maxEig, Ur-Ul);
			
			% Compute the final numerical flux.
			advec = Utility.timesArray(n(1,:), 0.5*(o.f(Ul,1) + o.f(Ur,1))) ...
				  + Utility.timesArray(n(2,:), 0.5*(o.f(Ul,2) + o.f(Ur,2)));
			ret = advec - diffusion;
		end
	end
end