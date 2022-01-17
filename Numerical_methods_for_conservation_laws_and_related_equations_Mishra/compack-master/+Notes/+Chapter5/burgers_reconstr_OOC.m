function table = burgers_reconstr_OOC
	% Set configurations
	conf = Configuration();
	conf.model = Model.Burgers;
	conf.useCellAvg = false;
	conf.timeInt = @TimeIntegration.RK2;
	conf.tMax = 1;
	conf.CFL = 0.8;		
	conf.bc = Mesh.BC.Neumann;
	conf.mesh = Mesh.Cartesian([-1,1], 1);
	conf.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_MM);

	conf.initial = @(x) x<0;
	
	schemes = { Flux.Burgers.Godunov, Flux.Rusanov };
	
	%%% Calculate rate of convergence
	% Exact solution of the Burgers equation
	exact = @(mesh, t, varargin) mesh.x < 0.5;
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