classdef MeshBase < handle
	%MESHBASE   Interface for mesh classes.
	
	
	properties (Abstract)
		internal	% Index into the internal of the grid
	end
	
	
	properties
		ndims		% Number of spatial dimensions
		nelem		% Number of elements in solution vector
		xLim		% Bounding box for the domain. For instance, 
					% xLim=[-1,1] is the domain x\in[-1,1], while xLim=[0,1;
					% 0,1] is the unit rectangle in R^2.
		xRange		% Size of domain in each direction
		x			% Computational grid. In 1D this is a vector of 
					% coordinates, while in 2D, it is a cell array of size 2
					% with x- and y-coordinates in each of the components.
		nx			% Number of gridcells in each direction (row vector)
		nxTot       % Total number of gridcells
		ngc = 0		% Number of ghost cells
		dx			% Grid size in each direction (cell array)
        dxMin		% Smallest grid size in the mesh in each direction (array)
	end
	
	
	
	methods
		% removeGhostCells()
		% Removes the ghost cells from a solution array U. If the ghost cells
		% have already been removed then the input U is returned.
		function U = removeGhostCells(o, U)
			if numel(U(1,:)) == o.nxTot
				% If the ghost cells have already been removed then do nothing.
				return;
			end
			U = U(:, o.internal);
			U = reshape(U, [o.nelem, o.nx]);
		end
	end
	
	
	methods (Abstract)
		% initialize()
		% Run by runSolver() to ensure that the mesh object is initialized.
		initialize(o, config)
		
		
		% netFlux()
		% Computes the net flux through the boundary of each element, as
		% specified by the numerical flux function 'numFlux'.
		%
		% Arguments:
		% numFlux:     Cell array of numerical flux functions
		% U:           Current cell averages
		% UReconstr:   Reconstructed values
		% t:           Current time
		% dt:          Current time step
		ret = netFlux(o, numFlux, U, UReconstr, t, dt)
		
		
		% newMesh()
		% Returns a new mesh
		ret = newMesh(o)		
		
		
		% newMeshElem()
		% Returns a new mesh with k elements at each point. By default, k=1.
		ret = newMeshElem(o, k)
				
		
		% evalFunc()
		% Evaluates a function "func" at all points of the mesh. If "cellAvg"
		% is true, then the function is averaged over each cell instead of
		% using point-wise evaluation.
		%
		% Arguments:
		% func:		Function to evaluate
		% cellAvg:	If true, then cell averages will be computed. If
		% 			false, then the function will be evaluated
		% 			at midpoints.
		ret = evalFunc(o, func, cellAvg)
	end	
	
	
end