classdef EC < Flux.NumFlux
	%EC   Entropy conservative 3-point flux for Linear advection equation 
	% with the entropy E(u) = u²/2
	
	
	properties
		name = 'EC'
	end
	
	
	methods		
		function ret = F(o, d, Ul, Ur, varargin)
			ret = o.model.a(d)*(Ul + Ur) / 2;
		end
	end
end