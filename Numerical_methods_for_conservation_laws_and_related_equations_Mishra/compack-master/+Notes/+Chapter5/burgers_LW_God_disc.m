function burgers_LW_God_disc
	conf = Configuration;
	conf.model = Model.Burgers;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 1;
	conf.CFL = 0.8;
	conf.initial = @(x) (x<0);
	conf.bc = Mesh.BC.Neumann;
	conf.mesh = Mesh.Cartesian([-1,1], 50);

	% Run Lax-Wendroff
	conf.solver = Flux.Burgers.LaxWen;
	solnLW = runSolver(conf);
	[t, uLW] = solnLW.getFinal();

	% Run Godunov
	conf.solver = Flux.Burgers.Godunov;
	solnGod = runSolver(conf);
	[t, uGod] = solnGod.getFinal();

	% Exact solution
	x = linspace(-1,1,1000);
	u = x < 0.5;
	
	% Display data	
	fig = figure();
	plot(x, u, 'k-');
	hold on;
	Plot.plotFrame(solnLW, uLW, t, 0, fig, '', 'bo-');
	Plot.plotFrame(solnGod, uGod, t, 0, fig, '', 'rs-');
	legend({'Exact', 'Lax-Wendroff', 'Godunov'}, 'Location', 'SouthWest');
	title('');
	axis tight;
	Plot.makeNice();
	Plot.saveFig('burgers_LW,God_disc_comparison');
end