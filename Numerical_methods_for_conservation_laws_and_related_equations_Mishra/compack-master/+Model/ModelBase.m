classdef ModelBase < handle
	%MODEL   Abstract base class for all model classes.
	
	properties (Abstract, SetAccess = private)
		name	% Name of model
		nelem	% Number of elements in the solution vector
	end
	
	
	methods
		% Calculates the maximum allowable time step size for the current
		% solution U.
		function ret = calcTimestep(o, U, mesh)
			reciprocal = 0;
			for m = 1:mesh.ndims
				maxEig = max(max(o.maxEig(U, m)));
				reciprocal = reciprocal + maxEig / mesh.dxMin(m);
			end
			ret = 1 / reciprocal;
		end

		
		% breaksPositivity()
		% Returns true if some sort of positivity condition is broken, for
		% instance if the density in the Euler equations is negative.
		% Default return value is false.
		function ret = breaksPositivity(o, U)
			ret = false;
		end
		
		
		% maxEig()
		% Computes the maximum (in absolute value) eigenvalue of the flux 
		% in direction d.
		% Arguments:
		% d:  Either 
		%     1) An integer between 1 and mesh.ndims (so that d=1 corresponds to
		%     f(U), d=2 to g(U), etc.), or
		%     2) An array of vectors specifying the direction n of the flux F.n.
		function ret = maxEig(o, U, d)
			if isscalar(d)
				ret = o.maxEigRect(U, d);
			else
				ret = o.maxEigDir(U, d);
			end
		end
	end
	
	
	methods (Static)
		% getVariable()
		% Extracts the component with name 'varname' from U.
		%
		% Input: 
		% soln:		Solution object returned from runSolver() 
		% U:		Solution at a specific point in time, obtained from
		%			getAtTime()	or getFinal().
		% varname:	(Optional) Name of variable to return. Default is varname=1,
		%			the first component.
		function ret = getVariable(soln, U, varname)
			if nargin < 3
				varname = 1;
			end			
			model = soln.config.model;			
			isIndex = isnumeric(varname) && isreal(varname) && (round(varname)==varname) && varname>=1 && varname<=model.nelem;
			assert(ischar(varname) || isIndex, 'Parameter ''varname'' must either be a string or a valid index integer.');

			% Extract the internal (excluding ghost cells) of the mesh
			U = soln.mesh.removeGhostCells(U);

			% Default values: Return the n-th component			
			if isIndex
				% 'varname' is an index number, return the corresponding
				% component.
				ret = U(varname, :, :);
			elseif strcmpi(varname, '')
				ret = U(1, :, :);
			else
				ret = model.doGetVariable(soln, U, varname);
			end
		end
	end
	
		
	methods (Abstract)
		% Flux function in a given direction
		%
		% Input:
		% U:	The array of conserved variables
		% dir:	Either 1 or 2, indicating x- or y-direction.
		ret = f(o, U, dir);
	end
	
	
	methods(Abstract, Access=protected)
		% maxEigRect()
		% maxEig() for rectangular meshes. 'dir' is either 1 or 2, indicating x-
		% or y-direction.
		ret = maxEigRect(o, U, dir);
	end
	
	
	methods (Access=protected)
		% maxEigDir()
		% Default implementation: use maxEigRect()
		function ret = maxEigDir(o, U, n)
			ret = abs(dot([o.maxEigRect(U,1); o.maxEigRect(U,2)], n, 1));
		end
		
		
		% doGetVariable()
		% Does the actual variable computation. Default behavior is to return
		% the first component of U.
		function ret = doGetVariable(o, soln, U, varname)
			ret = U(1,:,:);
		end
	end
end