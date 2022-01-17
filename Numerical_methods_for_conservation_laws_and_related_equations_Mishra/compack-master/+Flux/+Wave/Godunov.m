classdef Godunov < Flux.NumFlux
	%GODUNOV   Godunov flux for the wave equation
	%	In F() and G(), the matrix multiplication
	%		R*|Gamma|*R^(-1)*(Ur - Ul)
	%	has been precomputed. The matrix R*|Gamma|*R^(-1) takes the simple
	%	form 
	%		[c, 0, 0; 0, c, 0; 0, 0, 0]
	%	in the x-direction and
	%		[c, 0, 0; 0, 0, 0; 0, 0, c]
	%	in the y-direction.
	
	
	properties
		name = 'Godunov';
	end
	
	
	methods
		function ret = F(o, d, Ul, Ur, varargin)
			if d == 1
				ret = o.Fx(Ul, Ur);
			else
				ret = o.Fy(Ul, Ur);
			end
		end
	end
	
	
	methods (Access=private)
		function ret = Fx(obj, Ul, Ur)
			c = obj.model.c;
			diff = Ur - Ul;
			ret = 0.5*(obj.f(Ul, 1)+obj.f(Ur, 1)) - ...
				0.5*c*[diff(1,:,:); diff(2,:,:); zeros(size(diff(1,:,:)))];
		end
		
		
		function ret = Fy(obj, Ul, Ur)
			c = obj.model.c;
			diff = Ur - Ul;
			ret = 0.5*(obj.f(Ul, 2)+obj.f(Ur, 2)) - ...
				0.5*c*[diff(1,:,:); zeros(size(diff(1,:,:))); diff(3,:,:)];
		end
	end
	
end

