function burgers_sine_reconstr
	conf = Configuration;
	conf.model = Model.Burgers;
	conf.timeInt = @TimeIntegration.RK2;	
	conf.solver = Flux.Burgers.Godunov;
	conf.CFL = 0.8;
	conf.tMax = 0.6;
	conf.tInclude = linspace(0, 0.6, 3);
	conf.initial = @(x) (1+sin(2*pi*x))/2;
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([0, 1], 50);
	conf.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_MM);


	% Run solver
	soln = runSolver(conf);
	% Compute reference solution
	conf.mesh = Mesh.Cartesian([0, 1], 1000);
	solnExact = runSolver(conf);
		
	for i = 1:length(conf.tInclude)
		fig = figure();
		
		[t, uExact] = solnExact.getAtTime(conf.tInclude(i));
		Plot.plotFrame(solnExact, uExact, t, 0, fig, '', 'k-');
		hold on;
		[t, u] = soln.getAtTime(conf.tInclude(i));
		Plot.plotFrame(soln, u, t, 0, fig, '', 'bo-');
		title('');
		legend({'Exact', 'Godunov'}, 'Location', 'SouthWest');
		Plot.makeNice();
		Plot.saveFig(sprintf('burgers_sine_t=%g', t));
	end
end