function [nx, err, ooc] = linAdv_Upw_OOC
	%% Set configurations
	conf = Configuration();

	conf.model = Model.LinAdv;
	conf.solver = Flux.LinAdv.Upwind;
	conf.useCellAvg = false;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 10;
	conf.CFL = 0.8;		
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([0,1], 1);	
	conf.initial = @(x) sin(4*pi*x);


	%% Calculate rate of convergence
	% Exact solution of the linear advection equation
	exact = Error.exact_linAdv(conf.initial);
	% Which mesh sizes to compute over
	nx = 10 * 2.^(1:8);
	% Calculate the approximate rate of convergence.
	[dx, err, ooc] = Error.calcOOC(conf, exact, nx, 1, true);
	Plot.printLatexTable([nx; 100*err; [0, ooc]]', 3);
end