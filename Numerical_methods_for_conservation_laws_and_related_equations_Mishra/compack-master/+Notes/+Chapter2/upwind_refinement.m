function soln = upwind_refinement
	%% Set configurations and run the experiment on a coarse mesh
	conf = Configuration;

	% Create the model object and set the advection speed to 1
	conf.model = Model.LinAdv;
	conf.model.a = 1;
	conf.solver = Flux.LinAdv.Upwind;
	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 10;
	conf.CFL = 0.8;
	
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([0,1], 50);

	conf.initial = @(x) sin(2*pi*x);
	
    soln = runSolver(conf);


	%% Display data
	[t, U] = soln.getFinal();
	Plot.plotFrame(soln, U, t, 0, figure(), '', 'bo-');
	
	% Plot exact solution
	x = linspace(0,1,500);
	hold on;
	plot(x, sin(2*pi*(x-10)), 'r-');

	title('');
	xlabel('x');
	legend('Upwind scheme', 'Exact solution');
	Plot.makeNice();
 	Plot.saveFig('central_numx=50');
	
	
	%% Run the experiment again on a finer mesh
	conf.mesh = Mesh.Cartesian([0,1], 200);
	soln = runSolver(conf);
	[t, U] = soln.getFinal();
	Plot.plotFrame(soln, U, t, 0, figure(), '', 'bo-');
	
	% Plot exact solution
	x = linspace(0,1,500);
	hold on;
	plot(x, sin(2*pi*(x-1)), 'r-');

	Plot.makeNice();
	title('');
	xlabel('x');
	legend('Upwind scheme', 'Exact solution');
 	Plot.saveFig('central_numx=200');
end