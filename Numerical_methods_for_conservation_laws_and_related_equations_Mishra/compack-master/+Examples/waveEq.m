function soln = waveEq
	%% Set configurations
	conf = Configuration();

	conf.model = Model.Wave;
	conf.solver = Flux.Rusanov;

	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 2;
	conf.CFL = 0.4;
	
	
	function ret = initial(x,y)
		u = cos(pi*(x+y)) - cos(pi*(x-y));
		ret = [zeros(size(x)); u; u];
	end
	conf.initial = @initial;
	
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([-1,1; -1,1], [50,50]);


	%% Run solver
    soln = runSolver(conf);


	%% Display data, etc.
	Plot.plotSolution(soln, [-2,2], 'm1');
end