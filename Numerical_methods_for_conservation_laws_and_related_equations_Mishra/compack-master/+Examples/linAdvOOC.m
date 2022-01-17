function linAdvOOC
	%%% Set configurations
	conf = Configuration();
	conf.model = Model.LinAdv;
	conf.solver = Flux.Rusanov;
	conf.timeInt = @TimeIntegration.RK2;
	conf.tMax = 0.5;
	conf.CFL = 0.8;	
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([-1,1], 1);
	conf.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_MM);
	conf.initial = @(x) sin(2*pi*x);


	%%% Calculate rate of convergence
	% Exact solution of the linear advection equation
	exact = Error.exact_linAdv(conf.initial);
	% Which mesh sizes to compute over
	nx = 100:100:500;
	% Which L^p norms to compute
	p = [1, inf];
	% Calculate the approximate rate of convergence.
	Error.calcOOC(conf, exact, nx, p);
end