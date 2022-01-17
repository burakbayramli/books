function burgers_refine( )
	conf = Configuration;
	conf.model = Model.Burgers;
	conf.solver = Flux.Burgers.Godunov;	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 0.5;
	conf.CFL = 0.8;
	conf.bc = Mesh.BC.Neumann;
	conf.initial = @(x) -1 + 2*(x>0);

	numX = [20, 40, 80, 160];
	for i=1:4
		conf.mesh = Mesh.Cartesian([-1,1], numX(i));
		soln = runSolver(conf);

		% Display data
		[t, U] = soln.getFinal();
		fig=figure();
		plot([-1 -0.5 0.5 1], [-1 -1 1 1], 'k');
		hold on;
		Plot.plotFrame(soln, U, t, [-1.1, 1.1], fig, '', 'b-o');
		legend({'Exact solution', 'Approximate solution'}, 'Location', 'NorthWest');
		title('');
		Plot.makeNice;
		Plot.saveFig(sprintf('burgers_refine_nx=%d', numX(i)));
	end
end

