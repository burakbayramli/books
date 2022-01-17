function table = burgers_sine_reconstr_OOC
	% Set configurations
	conf = Configuration();
	conf.model = Model.Burgers;
	conf.useCellAvg = false;
	conf.timeInt = @TimeIntegration.RK2;
	conf.tMax = 0.3,
	conf.CFL = 0.8;		
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([0,1], 1);
	conf.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_MM);

	conf.initial = @(x) (1+sin(2*pi*x))/2;
	
	schemes = { Flux.Burgers.Godunov, Flux.Rusanov };
	
	%%% Calculate rate of convergence
	% Exact solution of the linear advection equation
	exact = Error.exact_burgers(conf.initial);
	% Which mesh sizes to compute over
	nx = 10 * 2.^(1:8);
	nnx = length(nx);
	
	% "table" will be the error table that is printed to screen
	table = zeros(nnx, 5);
	table(:,1) = nx;
	for i = 1:2
		conf.solver = schemes{i};
		% Calculate the approximate rate of convergence.
		[dx, err, ooc] = Error.calcOOC(conf, exact, nx, 1, true);
		table(:,2*i) = 100*err;
		table(:,2*i+1) = [0, ooc];
	end
	Plot.printLatexTable(table, 3);
end