function soln = SWBotTop
%SWBOTTOP   Example script for shallow water equations with variable bottom
% topography.

	%% Set configurations
	conf = Configuration();
	conf.model = Model.SW;
	conf.solver = Flux.SW.Roe;
	conf.useCellAvg = false;
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 5;
	conf.CFL = 0.9;
	conf.maxNumWrite = 300;
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([-1,1], 200);
	
	b = @(x) 0.2*(sin(2*pi*x)+1);
	function ret = initial(x)
		m = zeros(size(x));
		ret = [1 + 0.2*(abs(x)<0.3) - b(x); m; m];
	end
	conf.source = Source.SWBotTop(b);
	conf.initial = @initial;
	


	%% Run solver
    soln = runSolver(conf);


	%% Display data, etc.
	Plot.plotSolution(soln, [0,1.5], 'h+b');	
end