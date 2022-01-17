function soln = burgers_disc_error( )
    solvers = { @Flux.Burgers.Godunov, @Flux.Burgers.Roe, ...
         @Flux.LaxFr, @Flux.Rusanov, @Flux.Burgers.EngOsh };
	% Use mesh sizes 20, 40, ..., 5120.
 	nx = 10 * 2.^(1:9);
    ns = length(solvers);
    nnx = length(nx);
    dx = zeros(1, nnx);
    errors1 = zeros(ns, nnx);
    time1 = zeros(ns, nnx);
    errors2 = zeros(ns, nnx);
    time2 = zeros(ns, nnx);
    solvernames = cell(ns, 1);
    
    for j = 1:nnx
        for i = 1:ns
            %% Set configurations
            conf = Configuration();

            conf.model = Model.Burgers;
            conf.solver = solvers{i}();
			conf.useCellAvg = false;

            conf.timeInt = @TimeIntegration.FE;
            conf.tMax = 1;
            conf.CFL = 0.8;

            conf.bc = Mesh.BC.Neumann;
            conf.mesh = Mesh.Cartesian([-1,1], nx(j));	
            conf.initial = @(x) x<0;


            %% Run solver
            soln = runSolver(conf);
            time1(i,j) = soln.timeElapsed;
            solvernames{i} = conf.solver.name;


            %% Compute error
            [t, u] = soln.getFinal();            
            ind1 = (1:nx(j)) <= nx(j)/4 * 3;
            dx(j) = soln.mesh.dxMin;
            errors1(i, j) = dx(j) * (sum(abs(u(ind1)-1)) + sum(abs(u(~ind1))));


            %% Experiment 2
            conf = Configuration();

            conf.model = Model.Burgers;
            conf.solver = solvers{i}();
			conf.useCellAvg = false;

            conf.timeInt = @TimeIntegration.FE;
            conf.tMax = 0.5;
            conf.CFL = 0.8;

            conf.bc = Mesh.BC.Neumann;
            conf.mesh = Mesh.Cartesian([-1,1], nx(j));
            conf.initial = @(x) -1 + 2*(x>0);


            %% Run solver
            soln = runSolver(conf);
            time2(i,j) = soln.timeElapsed;


            %% Compute error
            [t, u] = soln.getFinal();
            ind1 = (1:nx(j)) <= nx(j)/4;
            ind3 = (1:nx(j)) >= nx(j)/4 * 3;
            ind2 = ~ind1 & ~ind3;
            uEx2 = soln.mesh.x(ind2) * 2;
            dx(j) = soln.mesh.dxMin;
            errors2(i, j) = dx(j) * (sum(abs(u(ind1)+1)) ...
                + sum(abs(u(ind2) - uEx2)) + sum(abs(u(ind3)-1)));
        end
    end
    
    
    figs = [figure() figure() figure() figure()];
    style = { 'o-', 'or-', 'og-', 'om-', 'oc-' };
    for j=1:ns
        set(0, 'CurrentFigure', figs(1));
        loglog(nx, errors1(j,:), style{j});
		hold on;

        set(0, 'CurrentFigure', figs(2));
        loglog(time1(j,:), errors1(j,:), style{j});
		hold on;

        set(0, 'CurrentFigure', figs(3));
        loglog(nx, errors2(j,:), style{j});
		hold on;

        set(0, 'CurrentFigure', figs(4));
        loglog(time2(j,:), errors2(j,:), style{j});
		hold on;
	end
	
	
	set(0, 'CurrentFigure', figs(1));
	xlabel('nx');
	ylabel('L^1 error');
	grid on;
	axis tight;
	legend(solvernames);
	Plot.makeNice();
	Plot.saveFig('error_disc_nx');
	
	set(0, 'CurrentFigure', figs(2));
	xlabel('Runtime (s)');
	ylabel('L^1 error');
	grid on;
	axis tight;
	legend(solvernames);
	Plot.makeNice();
	Plot.saveFig('error_disc_runtime');
	
	set(0, 'CurrentFigure', figs(3));
	xlabel('nx');
	ylabel('L^1 error');
	grid on;
	axis tight;
	legend(solvernames);
	Plot.makeNice();
	Plot.saveFig('error_raref_nx');
	
	set(0, 'CurrentFigure', figs(4));
	xlabel('Runtime (s)');
	ylabel('L^1 error');
	grid on;
	axis tight;
	legend(solvernames);
	Plot.makeNice();
	Plot.saveFig('error_raref_runtime');
end