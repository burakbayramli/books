classdef EC < Flux.NumFlux
%EC   Entropy conservative 3-point flux for Burgers' equation with the
% entropy E(u) = u²/2, on triangular meshes.
	
	
	properties
		name = 'EC'
	end
	
	
	methods		
		function ret = F(o, n, Ul, Ur, varargin)
% 			ret = ((Ul.*(Ul + Ur) + Ur.*Ur) / 6).*(n(1,:)+n(2,:));
			ret = Utility.timesArray(n(1,:)+n(2,:), (Ul.*(Ul + Ur) + Ur.*Ur) / 6);
		end
	end
	
end