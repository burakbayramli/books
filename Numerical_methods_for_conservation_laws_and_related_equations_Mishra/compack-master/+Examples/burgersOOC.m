function burgersOOC
	%% Set configurations
	conf = Configuration();
	conf.model = Model.Burgers;
	conf.solver = Flux.LaxFr;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 0.3;
	conf.CFL = 0.8;
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([-1,1], 100);	
	conf.initial = @(x) 1 + 0.5*sin(pi*x);


	%% Calculate rate of convergence
	exact = Error.exact_burgers(conf.initial);
	nx = [50, 100, 200, 300, 400, 500];
	p = [1, inf];
	Error.calcOOC(conf, exact, nx, p);
end