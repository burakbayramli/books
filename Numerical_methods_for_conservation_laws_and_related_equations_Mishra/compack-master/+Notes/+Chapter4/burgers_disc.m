function soln = burgers_disc( )
    solvers = { @Flux.Burgers.Godunov, @Flux.Burgers.Roe, ...
        @Flux.LaxFr, @Flux.Rusanov, @Flux.Burgers.EngOsh };
	solvernames = { 'Godunov', 'Roe', 'Lax-Friedrichs', 'Rusanov', 'Engquist-Osher' };
	
	figCompare = figure();
	plot([-1 0.5 0.5 1], [1 1 0 0], 'k', 'LineWidth', 2');
    hold on;
    ylim([-0.1 1.1]);
	style = { 'o-', 'or-', 'og-', 'om-', 'oc-' };
    
	%%% Set configurations
	conf = Configuration();

	conf.model = Model.Burgers;
	conf.timeInt = @TimeIntegration.FE;
	conf.CFL = 0.8;
	conf.bc = Mesh.BC.Neumann;	
	conf.mesh = Mesh.Cartesian([-1,1], 50);
	
	
	for i = 1:length(solvers)
		conf.solver = solvers{i}();
		
		
        %%% Experiment 1
		conf.tMax = 1;
		conf.initial = @(x) x<0;
        soln = runSolver(conf);

        fig = figure();

        % Plot exact solution
        plot([-1 0.5 0.5 1], [1 1 0 0], 'k');
        hold on;

        % Plot approximate solutions
        [t, U] = soln.getFinal();
        Plot.plotFrame(soln, U, t, 0, fig, '', 'ro-');
        title('');
        ylim([-0.1 1.1]);

        legend({'Exact', solvernames{i}}, 'Location', 'SouthWest');
		Plot.makeNice();
        Plot.saveFig(['burgers-', solvernames{i}, '_disc']);
		
		% Plot solution in comparison figure
		Plot.plotFrame(soln, U, t, 0, figCompare, '', style{i});
        title('');
        ylim([-0.1 1.1]);



        %%% Experiment 2
        conf.tMax = 0.5;
        conf.initial = @(x) -1 + 2*(x>0);
        soln = runSolver(conf);

        fig = figure();

        % Plot exact solution
        plot([-1 -0.5 0.5 1], [-1 -1 1 1], 'k');
        hold on;

        % Plot approximate solutions
        [t, U] = soln.getFinal();
        Plot.plotFrame(soln, U, t, 0, fig, '', 'ro-');
        title('');
        ylim([-1.1 1.1]);

        legend({'Exact', solvernames{i}}, 'Location', 'NorthWest');
		Plot.makeNice();
        Plot.saveFig(['burgers-', solvernames{i}, '_raref']);
	end
	
	
	set(0, 'CurrentFigure', figCompare);
	legend({'Exact', solvernames{:}}, 'Location', 'SouthWest');
	Plot.makeNice();
	Plot.saveFig('burgers-comparison_disc');
	xlim([0.15, 0.85]);
	Plot.saveFig('burgers-comparison_disc_comparison');
end