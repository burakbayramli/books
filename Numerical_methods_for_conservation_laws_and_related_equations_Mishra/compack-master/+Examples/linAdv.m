function soln = linAdv
	%%% Set configurations
	conf = Configuration;

	% Create the model object and set the advection speed to 1
	conf.model = Model.LinAdv;
	conf.model.a = 1;

	conf.solver = Flux.Rusanov;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 2;
	conf.CFL = 0.4;	
	conf.mesh = Mesh.Cartesian([-1,1], 200);
	conf.bc = Mesh.BC.Periodic;
	conf.initial = @(x) sin(2*pi*x);
	

	%%% Run solver
    soln = runSolver(conf);


	%%% Display data, etc.
	Plot.plotSolution(soln, [-1,1]);
end