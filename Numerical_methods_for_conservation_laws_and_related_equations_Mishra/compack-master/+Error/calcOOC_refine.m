function [ dx, err, ooc ] = calcOOC_refine( config, exact, mesh0, n, p, relative )
%CALCOOC_REFINE   General-purpose function for computing order of convergence.
%   Similar to calcOOC(), but takes any (Cartesian, Rect or Tri) mesh 'mesh0'
%   and a refinement number n>=1, and refines the mesh (i.e., halves the grid
%   size) n times, computing errors on each mesh.
%
%   Arguments:
%   config:   Initialized Configuration object
%   exact:    Function that computes the exact solution
%   nx        Vector of grid sizes to be computed on
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
	error(nargchk(5, 6, nargin));
	assert(n >= 1);	
	config.verbose = false;		% Turn off verbose mode
	np = length(p);
	mesh = mesh0;
	if nargin == 5
		relative = false;
	end
	
	err = zeros(np, n+1);
	ooc = zeros(np, n);
	dx = zeros(1, n+1);
	nx = zeros(1, n+1);
		
	
	
	%% Run experiments and calculate errors
	for k = 1:n+1
		nx(k) = mesh.nxTot;
		fprintf('nx = %i\n', nx(k));
		config.mesh = mesh;
		soln = runSolver(config);
		
		dx(k) = mesh.dxMin(1);
		[t, U] = soln.getFinal();
		uApprox = soln.getVariable(U, '');
		uExact = exact(mesh, t, config.useCellAvg);
		%TODO: Fix the following ugly hack
		if ~all(size(uExact) == size(uApprox))
			uExact = uExact(:,mesh.internal);
		end
		uDiff = uExact - uApprox;
		
		for j = 1:np
			err(j, k) = Error.calcNorm(mesh, uDiff, p(j));
			if relative
				err(j, k) = err(j, k) / Error.calcNorm(mesh, uExact, p(j));
			end
		end
		
		% Refine the mesh
		if k <= n
			mesh = mesh.refine();
		end
		
		% Delete the solution object
		clear soln;
	end
	
	
	%% Calculate the order of convergence
	for j = 1:np
		ooc(j,:) = log(err(j,1:n) ./ err(j,2:n+1)) ./ log(dx(1:n) ./ dx(2:n+1));
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
