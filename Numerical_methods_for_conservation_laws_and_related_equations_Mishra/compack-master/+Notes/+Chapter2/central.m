function soln = central
	%% Set configurations
	conf = Configuration;

	% Create the model object and set the advection speed to 1
	conf.model = Model.LinAdv;
	conf.model.a = 1;
	conf.solver = Flux.Central;
	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 3;
	conf.CFL = 0.5;
	
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([0,1], 100);

	conf.initial = @(x) sin(2*pi*x);
	

	%% Run solver
    soln = runSolver(conf);


	%% Display data, etc.
	[t, U] = soln.getFinal();
	Plot.plotFrame(soln, U, t, 0, figure(), '');

	title('');
	xlabel('x');
	Plot.makeNice();
	Plot.saveFig('central');
end