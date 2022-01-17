function [nx, err, ooc] = burgers_OOC
	%% Set configurations
	conf = Configuration();

	conf.model = Model.Burgers;
	conf.solver = Flux.Burgers.Godunov;
	conf.useCellAvg = false;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 0.5;
	conf.CFL = 0.8;		
	conf.bc = Mesh.BC.Neumann;
	conf.mesh = Mesh.Cartesian([-1,1], 1);
	conf.initial = @(x) -1 + 2*(x>0);


	%% Calculate rate of convergence
	% Exact solution of the linear advection equation
	exact = @(mesh, varargin) Utility.clamp(2*mesh.x, [-1, 1]);
	% Which mesh sizes to compute over
	nx = 10 * 2.^(1:8);
	% Calculate the approximate rate of convergence.
	[dx, err, ooc] = Error.calcOOC(conf, exact, nx, 1, true);
	Plot.printLatexTable([nx; 100*err; [0, ooc]]', 3);
end