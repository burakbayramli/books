function table = linAdv_reconstr_OOC
	% Set configurations
	conf = Configuration();
	conf.model = Model.LinAdv;
	conf.useCellAvg = false;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 10;
	conf.CFL = 0.8;
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([0,1], 1);	
	conf.solver = Flux.LinAdv.Exact_2nd;
	conf.initial = @(x) sin(4*pi*x);
	
	slopeLims = { Reconstr.Lim_MM, Reconstr.Lim_MC, Reconstr.Lim_SB };
	
	%%% Calculate rate of convergence
	% Exact solution of the linear advection equation
	exact = Error.exact_linAdv(conf.initial);
	% Which mesh sizes to compute over
	nx = 10 * 2.^(1:8);
	nlim = length(slopeLims);
	nnx = length(nx);
	
	% "table" will be the error table that is printed to screen
	table = zeros(nnx, 2*nlim+1);
	table(:,1) = nx;
	for i = 1:nlim
		conf.reconstr = Reconstr.SlopeLimiter(slopeLims{i});
		% Calculate the approximate rate of convergence.
		[dx, err, ooc] = Error.calcOOC(conf, exact, nx, 1, true);
		table(:,2*i) = 100*err;
		table(:,2*i+1) = [0, ooc];
	end
	Plot.printLatexTable(table, 3);
end