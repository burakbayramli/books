classdef Exact < Flux.NumFlux
	properties
		name = 'EC'
	end
	
	
	methods
		function ret = F(o, n, Ul, Ur, t, varargin)
			x = (o.mesh.p(o.mesh.edges(:,1),:)' + o.mesh.p(o.mesh.edges(:,2),:)')/2;
			UEx = o.initial(x(1,:), x(2,:)-t);
			ret = UEx .* (o.model.a(1)*n(1,:) + o.model.a(2)*n(2,:));
		end
	end
	
	
	methods (Access=private)
		function U0 = initial(o, x, y)
			X = 4*[x; y+0.5];
			U0 = exp(1 ./ (dot(X,X) - 1))/exp(-1);
			U0(dot(X,X) >= 1) = 0;
		end
	end
end