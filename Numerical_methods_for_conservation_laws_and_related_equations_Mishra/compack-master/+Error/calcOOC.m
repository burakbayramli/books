function [ dx, err, ooc ] = calcOOC( config, exact, nx, p, relative )
%CALCOOC   General-purpose function for computing order of convergence.
%
%   Arguments:
%   config:   Initialized Configuration object
%   exact:    Function that computes the exact solution
%   nx        Vector of grid sizes to be computed on. If the mesh is 2D then
%             this should be an N*2 array with grid sizes in x- and y-directions
%             along the second dimension.
%   p:        Which L^p norm to use, e.g. p=1 or p=[1, 2, inf].
%   relative: (Optional) Whether to compute relative errors or not. Does
%			  not have an effect on the OOC, but does on the errors.
%			  Default is false.
%
%   Example:
%      exact = Error.exact_linAdv(config.initial);
%      [dx,err,ooc] = calcOOC(exact, config, 100:100:500, 1);
%   Calculates the order of convergence of the scheme specified by the
%   Configuration object 'config' over mesh sizes 100,200,...,500 in the
%   L^1 norm.


	%% Initialize
	assert(length(nx) > 1, 'Parameter ''nx'' must have length greater than 1.');	
	config.verbose = false;		% Turn off verbose mode
	np = length(p);
	% Check if nx is a 2-D array
	if size(nx,1)>1 && size(nx,2)>1
		assert(ndims(nx) == 2, 'Parameter ''nx'' must have 1 or 2 dimensions.');
		assert(size(nx,2) == 2, 'Parameter ''nx'' must be a vector or an n*2 array.');
		n = size(nx, 1);
	else
		nx = nx(:);		% Make sure 'nx' is a column vector
		n = length(nx);
	end
	err = zeros(np, n);
	ooc = zeros(np, n-1);
	dx = zeros(1, n);
	xLim = config.mesh.xLim;
	if nargin == 4
		relative = false;
	end
		
	
	
	%% Run experiments and calculate errors
	for k = 1:n
		fprintf('numX = %i\n', nx(k,1));
		mesh = Mesh.Cartesian(xLim, nx(k,:));
		config.mesh = mesh;
		soln = runSolver(config);
		
		dx(k) = mesh.dxMin(1);
		[t, U] = soln.getFinal();
		uApprox = soln.getVariable(U, 1);
		uExact = exact(mesh, t, config.useCellAvg);
		uExact = mesh.removeGhostCells(uExact);
		uDiff = uExact - uApprox;
		
		for j = 1:np
			err(j, k) = Error.calcNorm(soln.mesh, uDiff, p(j));
			if relative
				err(j, k) = err(j, k) / Error.calcNorm(soln.mesh, uExact, p(j));
			end
		end
		
		clear soln;
	end
	
	
	%% Calculate the order of convergence
	for j = 1:np
		ooc(j,:) = log(err(j,1:n-1) ./ err(j,2:n)) ./ log(dx(1:n-1) ./ dx(2:n));
	end
	
	
	%% Output to console
	fprintf('\n');	
	for j = 1:np
		fprintf('L^%i error:', p(j));
		fprintf('    %e', err(j,:));
		fprintf('\n');
		fprintf('L^%i order:', p(j));
		fprintf('    %.4f', ooc(j,:));
		fprintf('\n');
		fprintf('Average order:    %.4f\n', sum(ooc(j,:))/length(ooc(j,:)));
		fprintf('\n');
		
		% Convergence plot
		figure();		
		loglog(nx, err(j,:), '-o');
		xlabel('Number of grid points');
		ylabel(sprintf('L^{%i} error', p(j)));
		axis tight;
		title(['Solver: ' config.solver.name]);
	end	
end
