function soln = euler2D
	%% Set configurations
	conf = Configuration();

	conf.model = Model.Euler;
	conf.solver = Flux.Euler.Roe;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 1;
	conf.CFL = 0.9;
		
	% 2D example
	function ret = initial(x,y)
		r = sqrt((x).^2+y.^2);
		rho = ones(size(x));
		u = zeros(size(x));
		p = 1 + (r<0.5);
		[m1, m2, E] = conf.model.primToCons(rho, u, u, p);
		ret = [rho; m1; m2; E];
	end

	conf.initial = @initial;
	
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([-2,2;-2,2], [50,50]);


	%% Run solver
    soln = runSolver(conf);


	%% Display data, etc.
	Plot.plotSolution(soln, [0.7,1.5], 'p');	
end