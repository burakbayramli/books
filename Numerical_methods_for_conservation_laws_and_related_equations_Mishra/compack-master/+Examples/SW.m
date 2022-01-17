function soln = SW
	%% Set configurations
	conf = Configuration();
	conf.model = Model.SW;
	conf.solver = Flux.Rusanov;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 3;
	conf.CFL = 0.9;
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([-1,1; -1,1], [50,50]);
	conf.maxNumWrite = 500;
	
	function ret = initial(x,y)
		r = sqrt(x.*x + y.*y);
		m = zeros(size(x));
		ret = [1 + (r < 0.3); m; m];
	end
	conf.initial = @initial;
	


	%% Run solver
    soln = runSolver(conf);


	%% Display data, etc.
	Plot.plotSolution(soln, [0.7, 1.5], 'h');	
end