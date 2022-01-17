function linAdv_Upw_refine( )
	conf = Configuration;
	conf.model = Model.LinAdv;
	conf.model.a = 1;
	conf.solver = Flux.LinAdv.Upwind;	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 10;
	conf.CFL = 0.8;
	conf.bc = Mesh.BC.Periodic;
	conf.initial = @(x) sin(4*pi*x);
	
	xExact = linspace(0,1,1000);
	uExact = sin(4*pi*xExact);

	numX = [40, 80, 160, 320];
	for i=1:4
		conf.mesh = Mesh.Cartesian([0,1], numX(i));
		soln = runSolver(conf);

		% Display data
		[t, u] = soln.getFinal();
		fig=figure();
		plot(xExact, uExact, 'k-');
		hold on;
		Plot.plotFrame(soln, u, t, [-1, 1], fig, '', 'b-o');
		legend('Exact solution', 'Approximate solution');
		title('');
		Plot.makeNice;
		Plot.saveFig(sprintf('linAdv_sin_nx=%d', numX(i)));
	end
end

