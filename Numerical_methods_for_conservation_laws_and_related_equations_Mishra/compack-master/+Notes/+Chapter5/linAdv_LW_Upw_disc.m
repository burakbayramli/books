function linAdv_LW_Upw_disc
	conf = Configuration;
	conf.model = Model.LinAdv;
	conf.model.a = 1;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 1;
	conf.CFL = 0.8;
	conf.initial = @(x) 1 + (x<0.5);
	conf.bc = Mesh.BC.Neumann;
	
	
	for nx = [100, 1000]
		conf.mesh = Mesh.Cartesian([0,2], nx);
		
		% Run Lax-Wendroff
		conf.solver = Flux.LinAdv.LaxWen;
		solnLW = runSolver(conf);
		[t, uLW] = solnLW.getFinal();

		% Run Upwind
		conf.solver = Flux.LinAdv.Upwind;
		solnUpw = runSolver(conf);
		[t, uUpw] = solnUpw.getFinal();

		% Exact solution
		x = linspace(0,2,1000);
		u = 1 + (x < 1.5);

		% Display data
		fig = figure();
		plot(x, u, 'k-');
		hold on;
		Plot.plotFrame(solnLW, uLW, t, 0, fig, '', 'b-');
		Plot.plotFrame(solnUpw, uUpw, t, 0, fig, '', 'r-');
		legend({'Exact', 'Lax-Wendroff', 'Upwind'}, 'Location', 'SouthWest');
		title('');
		axis tight;
		Plot.makeNice();
		Plot.saveFig(sprintf('LW,upwind_disc_comparison_nx=%d', nx));
	end
end