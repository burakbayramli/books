function soln = upwind_refinement_disc
	%% Set configurations and run the experiment on a coarse mesh
	conf = Configuration;

	% Create the model object and set the advection speed to 1
	conf.model = Model.LinAdv;
	conf.model.a = 1;
	conf.solver = Flux.LinAdv.Upwind;
	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 0.25;
	conf.CFL = 0.8;
	
	conf.bc = Mesh.BC.Neumann;
	conf.mesh = Mesh.Cartesian([0,1], 50);

	conf.initial = @(x) 1 + (x<0.5);
	
    soln = runSolver(conf);


	%% Display data
	[t, U] = soln.getFinal();
	Plot.plotFrame(soln, U, t, 0, figure(), '', 'bo-');
	
	% Plot exact solution
	x = linspace(0,1,500);
	hold on;
	plot(x, 1 + (x<0.75), 'r-');

	Plot.makeNice();
	title('');
	xlabel('x');
	legend('Upwind scheme', 'Exact solution');
	ylim([1 2.4]);
 	Plot.saveFig('central_disc_numx=50');
	
	
	%% Run the experiment again on a finer mesh
	conf.mesh = Mesh.Cartesian([0,1], 200);
	soln = runSolver(conf);
	[t, U] = soln.getFinal();
	Plot.plotFrame(soln, U, t, 0, figure(), '', 'bo-');
	
	% Plot exact solution
	x = linspace(0,1,500);
	hold on;
	plot(x, 1 + (x<0.75), 'r-');

	Plot.makeNice();
	title('');
	xlabel('x');
	legend('Upwind scheme', 'Exact solution');
	ylim([1 2.3]);
 	Plot.saveFig('central_disc_numx=200');
end