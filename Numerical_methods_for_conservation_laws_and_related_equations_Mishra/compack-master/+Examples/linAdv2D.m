function soln = linAdv2D
	%%% Set configurations
	conf = Configuration();

	% Create the model object and set the advection speed in each direction
	% to 1
	conf.model = Model.LinAdv;
	conf.model.a = [1, 1];

	conf.solver = Flux.Rusanov;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 2;
	conf.CFL = 0.8;	
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([-1,1; -1,1], [50,50]);
	conf.initial = @(x,y) exp(-(x.*x+y.*y));
	

	%%% Run solver
    soln = runSolver(conf);


	%%% Display data
	Plot.plotSolution(soln, [0,1]);
	
end