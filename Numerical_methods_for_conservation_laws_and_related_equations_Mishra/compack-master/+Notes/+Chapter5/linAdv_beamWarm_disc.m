function linAdv_beamWarm_disc
	conf = Configuration;
	conf.model = Model.LinAdv;
	conf.model.a = 1;
	conf.timeInt = @TimeIntegration.FE;
	
	% The Beam-Warming scheme is the same as exact propagation + backwards
	% linear reconstruction + forward Euler time discretization.
	conf.solver = Flux.LinAdv.Exact_2nd;
	conf.reconstr = Reconstr.SlopeLimiter(Reconstr.Backward);
	
	conf.tMax = 1;
	conf.CFL = 0.8;
	conf.initial = @(x) 1 + (x<0.5);
	conf.bc = Mesh.BC.Neumann;
	conf.mesh = Mesh.Cartesian([0,2], 50);

	% Run Beam-Warming
	solnBW = runSolver(conf);
	[t, uBW] = solnBW.getFinal();

	% Exact solution
	x = linspace(0,2,1000);
	u = 1 + (x < 1.5);

	% Display data
	fig = figure();
	plot(x, u, 'k-');
	hold on;
	Plot.plotFrame(solnBW, uBW, t, 0, fig, '', 'bo-');
	legend({'Exact', 'Beam-Warming'}, 'Location', 'SouthWest');
	title('');
	axis tight;
	Plot.makeNice();
	Plot.saveFig('linAdv_beamWarm_disc');
end