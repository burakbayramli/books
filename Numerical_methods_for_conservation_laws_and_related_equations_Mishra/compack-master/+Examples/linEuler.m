function soln = linEuler
	%% Set configurations
	conf = Configuration();

	conf.model = Model.LinEuler;
	conf.solver = Flux.LinEuler.Roe;

	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 0.5;
	conf.CFL = 0.4;
	
	
	function ret = initial(x)
		rho = 1 - 0.8*(x>0);
		u = zeros(size(x));
		v = zeros(size(x));
		p = rho;
		ret = [rho; u; v; p];
	end
	conf.initial = @initial;
	
	conf.bc = Mesh.BC.Neumann;
	conf.mesh = Mesh.Cartesian([-1,1], 100);


	%% Run solver
    soln = runSolver(conf);


	%% Display data, etc.
	Plot.plotSolution(soln, 0, 'rho');
end