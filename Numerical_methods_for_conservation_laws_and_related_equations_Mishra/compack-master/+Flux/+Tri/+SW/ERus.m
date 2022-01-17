classdef ERus < Flux.NumFlux
%ERUS   ERus (entropy stable Rusanov-type) flux for the shallow water equations.
	
	properties(Constant)
		name = 'ERus'
	end
	
	properties (Access = private)
		flux
	end
	
	
	
	methods
		function o = ERus()
			o.flux = Flux.SW.Tri.EC;
		end
		
		
		function initialize(o, varargin)
			initialize@Flux.NumFlux(o, varargin{:});
			o.flux.initialize(varargin{:});
		end
		
		
		function ret = F(o, n, Ul, Ur, t, dt)
			c = max([o.maxEig(Ul, n); o.maxEig(Ur, n)]);
			diffusion = Utility.timesArray(c, Ur-Ul) / 2;
			ret = o.flux.F(n, Ul, Ur, t, dt) - diffusion;
		end
	end
end