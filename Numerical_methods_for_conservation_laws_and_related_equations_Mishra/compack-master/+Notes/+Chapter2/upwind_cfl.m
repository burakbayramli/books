function soln = upwind_cfl
	%% Set configurations and run the experiment with a bad CFL number
	conf = Configuration;

	% Create the model object and set the advection speed to 1
	conf.model = Model.LinAdv;
	conf.model.a = 1;
	conf.solver = Flux.LinAdv.Upwind;
	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 1;
	conf.CFL = 1.3;
	
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([0,1], 100);

	conf.initial = @(x) sin(2*pi*x);
	
    soln = runSolver(conf);


	%% Display data
	[t, U] = soln.getFinal();
	Plot.plotFrame(soln, U, t, 0, figure(), '', 'bo-');
	
	% Plot exact solution
	x = linspace(0,1,500);
	hold on;
	plot(x, sin(2*pi*(x-1)), 'r-');

	title('');
	xlabel('x');
	legend('Upwind scheme', 'Exact solution');
	Plot.makeNice();
 	Plot.saveFig('central_cfl_bad');
	
	
	%% Run the experiment again with a good CFL number
	conf.CFL = 0.9;
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
 	Plot.saveFig('central_cfl_good');
end