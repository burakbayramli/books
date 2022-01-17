classdef NumFlux < handle
	%NUMFLUX   Base class for numerical fluxes.
	
	properties
		model
		mesh
		reconstr	% Reconstruction method
		source
		ngc = 1;	% Number of ghost cells needed by the flux. The default is 1.
	end
	
	properties(Access = private)
		computeNetFlux
	end
	
	
	
	properties (Abstract)
		% Name of the solver (must be implemented by solvers)
		name
	end
	
	
	
	methods (Abstract)
		% F()
		% Numerical flux function. Takes left and right values  and returns
		% an approximate solution to the corresponding Riemann problem.
		%
		% Arguments:
		% dir:       Spatial direction. dir=1 indicates x-direction, dir=2
		%            y-direction.
		% Ul, Ur:    Left and right reconstructed values
		% t:         Current time
		% dt:        Current time step
		% UlA, UrA:  Left and right cell averages
		ret = F(o, dir, Ul, Ur, t, dt, UlA, UrA);
	end
	
	
	
	methods
		% netFlux()
		% Compute net flux through cell edges:
		%	ret(i) = (F_{i+1/2} - F_{i-1/2})/dx.
		% If there is a nonzero source term in the equation then this is
		% also evaluated and added.
		function ret = netFlux(o, U, t, dt)			
			% Create the net flux function, if it hasn't been done already.
			if isempty(o.computeNetFlux)
				if ~isempty(o.source)
					o.computeNetFlux = @(U_, UR_, t_, dt_) ...
						- o.mesh.netFlux(@o.F, U_, UR_, t_, dt_) ...
						+ o.source.source(U_, UR_, t_, dt_);
				else
					o.computeNetFlux = @(U_, UR_, t_, dt_) ...
						- o.mesh.netFlux(@o.F, U_, UR_, t_, dt_);
				end
			end
			
			% Reconstruct and compute net flux
			UReconstr = o.reconstr.reconstruct(U, o.mesh);
			ret = o.computeNetFlux(U, UReconstr, t, dt);
		end
		
		
		function initialize(o, model, mesh, reconstr, source)
			o.model = model;
			o.mesh = mesh;
			o.reconstr = reconstr;
			o.source = source;
		end
	end
	
	
	
	% Convenience functions for numerical fluxes
	methods (Access = protected)
		% f()
		% Returns the flux function in "dir"-direction evaluated at "U".
		% U:	State values
		% dir:	Either 1 or 2, indicating either x- or y-direction
		function ret = f(o, U, dir)
			ret = o.model.f(U, dir);
		end
		
		
		% maxEig()
		% Computes the maximum (in absolute value) eigenvalue of f'(U)
		function ret = maxEig(o, U, dir)
			ret = o.model.maxEig(U, dir);
		end
	end
end