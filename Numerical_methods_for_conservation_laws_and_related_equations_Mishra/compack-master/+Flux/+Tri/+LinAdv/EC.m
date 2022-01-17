classdef EC < Flux.NumFlux
%EC   Entropy conservative 3-point flux for the linear advection equation with
%	the entropy E(u) = u²/2, on triangular meshes.
	
	
	properties
		name = 'EC'
	end
	
	
	methods
		function ret = F(o, n, Ul, Ur, varargin)
% 			n(:,o.mesh.atEdge) = -n(:,o.mesh.atEdge);
			ret = ((Ul+Ur)/2) .* (o.model.a(1)*n(1,:) + o.model.a(2)*n(2,:));
			
% 			ne = length(Ul);
% 			atEdgeInd = false(ne,1);	atEdgeInd(o.mesh.atEdge) = true;
% 			for k = 1:ne
% 				if atEdgeInd(k)
% 				end
% 			end
		end
	end
end