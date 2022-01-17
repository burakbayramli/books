function soln = burgers_godunov_sine( )
	%% Set configurations
	conf = Configuration();

	conf.model = Model.Burgers;
	conf.solver = Flux.Burgers.Godunov;
	
	conf.timeInt = @TimeIntegration.FE;
	conf.tMax = 0.5;
	conf.CFL = 0.8;
	
	conf.bc = Mesh.BC.Periodic;
	conf.mesh = Mesh.Cartesian([-1,1], 100);
	conf.initial = @(x) sin(4*pi*x);


	%% Run solver
	soln = runSolver(conf);
    
    
    %% Compute reference solution
    conf.mesh = Mesh.Cartesian([-1,1], 5000);
    soln2 = runSolver(conf);


	%% Display data
	fig = figure();
	set(gca, 'FontSize', 12);
    
    % Plot exact solution
    [t, U] = soln2.getFinal();
	Plot.plotFrame(soln2, U, t, 0, fig, '', 'k');
	hold on;
   
	% Plot approximate solutions
	[t, U] = soln.getFinal();
	Plot.plotFrame(soln, U, t, 0, fig, '', 'ro-');
    title('');
	 
    legend({'Exact', 'Godunov'});
	Plot.makeNice();
    Plot.saveFig('burgers-godunov_sine');
end