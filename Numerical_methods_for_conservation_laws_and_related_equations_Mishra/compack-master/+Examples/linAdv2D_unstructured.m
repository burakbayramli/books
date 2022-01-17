function soln = linAdv2D_unstructured
%   Demonstration of unstructured tensorial 2D meshes with the linear advection
%   equation. The mesh is generated at random, and therefore very small
%   mesh sizes, and hence small timesteps (and long runtimes) may occur.
%
%   See also: Examples.linAdv_unstructured

	%% Set configurations
	conf = Configuration;
	conf.model = Model.LinAdv;
	conf.solver = Flux.Rusanov;	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 0.2;
	conf.CFL = 0.8;
	conf.bc = Mesh.BC.Periodic;
	conf.initial = @(x,y) exp(-(x.*x+y.*y));
	
	% Create a randomly spaced grid in the interval [-1,1]x[-1,1] with
	% 40x40 grid cells.
	x = [-1, 1, -1+2*rand(1,39)];
	y = [-1, 1, -1+2*rand(1,39)];
	conf.mesh = Mesh.Rect(x, y);


	%%% Run solver
    soln = runSolver(conf);


	%%% Display data
	Plot.plotSolution(soln, [0,1]);
end