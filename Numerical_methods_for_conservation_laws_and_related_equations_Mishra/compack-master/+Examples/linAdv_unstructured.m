function soln = linAdv_unstructured
%   Demonstration of unstructured 1D meshes with the linear advection
%   equation. The mesh is generated at random, and therefore very small
%   mesh sizes, and hence small timesteps (and long runtimes) may occur.


	%%% Set configurations
	conf = Configuration;
	conf.model = Model.LinAdv;
	conf.solver = Flux.Rusanov;	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 0.2;
	conf.CFL = 0.8;	
	conf.bc = Mesh.BC.Periodic;
	conf.initial = @(x) sin(2*pi*x);
	
	% Create a randomly spaced grid in the interval [0,1] with 100 grid
	% cells ('x' has 101 elements, as it gives the locations of the
	% cell interfaces and not the centers of each cell). The Mesh.Rect
	% constructor sorts the gridpoints automatically in increasing order.
	x = [0,1,rand(1,99)];
	conf.mesh = Mesh.Rect(x);


	%%% Run solver
    soln = runSolver(conf);


	%%% Display data
	Plot.plotSolution(soln, [-1,1]);
end