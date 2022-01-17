classdef ERus < Flux.NumFlux
%ERUS   Entropy stable scheme using Rusanov diffusion on triangular meshes
	
	properties
		ecFlux
		name = 'ERus'
	end
	
	
	methods
		function o = ERus(ecFlux)
			o.ecFlux = ecFlux;
		end
		
		
		% initialize()
		% Make sure both ERus.initialize() and ecFlux.initialize() are called.
		function initialize(o, model, mesh, reconstr, source)
			initialize@Flux.NumFlux(o, model, mesh, reconstr, source);
			o.ecFlux.initialize(model, mesh, reconstr, source);
		end
		
		
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
			advec = o.ecFlux.F(n, Ul, Ur, varargin{:});
			ret = advec - diffusion;
		end
	end
end