classdef Roe < Flux.NumFlux
	%ROE   Roe flux for the linearized Euler equations
	
	properties
		name = 'Roe'
	end
	
	methods
		function ret = F(o, d, Ul, Ur, varargin)
			[R, RInv] = o.model.eigenvectors(Ul, d);
			Gamma = o.model.eigenvalues(Ul, d);
			diffMatr = R*abs(Gamma)*RInv;
			
			% Advection part
			flux = 0.5*(o.f(Ul, d)+o.f(Ur, d));
			% Diffusion operator
			diffusion = zeros(size(Ul));
			for j = 1:numel(Ul(1,:))
				% Set the Roe diffusion operator
				diffusion(:, j) =  0.5*diffMatr*(Ur(:,j) - Ul(:,j));
			end
			ret = flux - diffusion;
		end
	end
end