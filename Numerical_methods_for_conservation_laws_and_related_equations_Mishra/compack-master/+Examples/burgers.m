function soln = burgers( )
	%% Set configurations
	conf = Configuration();

	conf.model = Model.Burgers;
	conf.solver = Flux.Burgers.Roe;
	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 1.2;
	conf.CFL = 0.4;
	
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([-1,1], 100);	
	conf.initial = @(x) 1 + 0.5*sin(pi*x);


	%% Run solver
	soln = runSolver(conf);


	%% Display data
	Plot.plotSolution(soln);	
end