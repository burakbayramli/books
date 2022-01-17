function linAdv_beamWarm_Upw_sine
	conf = Configuration;
	conf.model = Model.LinAdv;
	conf.model.a = 1;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 10;
	conf.CFL = 0.8;
	conf.initial = @(x) sin(4*pi*x);
	conf.mesh = Mesh.Cartesian([0,1], 50);
	conf.bc = Mesh.BC.Periodic;
  
  % Run Upwind
	conf.solver = Flux.LinAdv.Upwind;
    solnUpw = runSolver(conf);
	[t, uUpw] = solnUpw.getFinal();
	
	% Run Beam-Warming: 
  % Exact propagation + backwards linear reconstruction + forward Euler time discretization.
	conf.solver = Flux.LinAdv.Exact_2nd;
  conf.reconstr = Reconstr.SlopeLimiter(Reconstr.Backward);
    solnLW = runSolver(conf);
	[t, uLW] = solnLW.getFinal();
	
	% Exact solution
	x = linspace(0,1,1000);
	u = sin(4*pi*x);

	% Display data
	fig = figure();
	plot(x, u, 'k-');
	hold on;
	Plot.plotFrame(solnLW, uLW, t, 0, fig, '', 'bo-');
	Plot.plotFrame(solnUpw, uUpw, t, 0, fig, '', 'rs-');
	title('');
	legend({'Exact', 'Beam-Warming', 'Upwind'});
	Plot.makeNice();
	Plot.saveFig('BW, upwind_comparison');
end