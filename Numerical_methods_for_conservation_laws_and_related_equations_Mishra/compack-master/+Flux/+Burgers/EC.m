classdef EC < Flux.NumFlux
	%EC   Entropy conservative 3-point flux for Burgers' equation with the
	% entropy E(u) = u²/2
	
	
	properties
		name = 'EC'
	end
	
	
	methods		
		function ret = F(o, d, Ul, Ur, varargin)
			ret = (Ul.*(Ul + Ur) + Ur.*Ur) / 6;
		end
	end
end