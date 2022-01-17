classdef FluxLim < Flux.NumFlux
%FLUXLIM   Limited flux for scalar conservation laws.
	
	properties
		name = 'FluxLim'
	end
	
	
	properties(Access='private')
		phi
	end
	
	
	methods
		function o = FluxLim(phi)
			if nargin == 0
				% Default limiter: phi(r)=0
				o.phi = @(r) zeros(size(r));
			else
				o.phi = phi;
			end
			
			% Set number of ghost cells to 2
			o.ngc = 2;
		end
		
		
		
		function ret = F(o, d, Ul, Ur, t, dt, varargin)
			lambda = dt / o.mesh.dx{d};
			
			% Compute alpha, r, nu and phi(r)
			fl = o.f(Ul, d);
			fr = o.f(Ur, d);
			dU = Ur-Ul;
			nu = (fr-fl) ./ dU;
			nu(dU==0) = 0;
			alpha = 0.5*nu.*(1-lambda*nu);
			
			% Compute theta, the nominator/denominator alpha \Delta u of r
			theta = alpha .* dU;
			if d == 1
				thetaL = theta(:, 1:end-1, :);
				thetaR = theta(:, 2:end, :);
			else % d == 2
				thetaL = theta(:, :, 1:end-1);
				thetaR = theta(:, :, 2:end);
			end			
			r = thetaL ./ thetaR;
			r(thetaR == 0) = 0;
			
			% Compute the numerical flux function
			if d == 1
				ret = fl + cat(2, zeros(1,1,size(r,3)), o.phi(r).*thetaR);
			else % d == 2
				ret = fl + cat(3, zeros(1,size(r,2),1), o.phi(r).*thetaR);
			end
		end
	end
end