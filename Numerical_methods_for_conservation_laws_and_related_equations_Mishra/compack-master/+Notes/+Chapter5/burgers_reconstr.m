function burgers_reconstr
	conf = Configuration;
	conf.model = Model.Burgers;
	conf.timeInt = @TimeIntegration.RK2;	
	conf.solver = Flux.Burgers.Godunov;
	conf.CFL = 0.8;
	conf.tMax = 1;
	conf.initial = @(x) (x<0);
	conf.bc = Mesh.BC.Neumann;
	conf.mesh = Mesh.Cartesian([-1, 1], 50);

	
	
	% Exact solution
	xDisc = linspace(-1, 1,1000);
	uDisc = (xDisc < 0.5);
	fig = figure();
	plot(xDisc, uDisc, 'k-');
	hold on;
	
	slopeLims = { Reconstr.Lim_MM, Reconstr.Lim_MC, Reconstr.Lim_SB };
	names = {'Minmod', 'MC', 'Superbee'};	
	styles = {'bo-', 'ro-', 'go-'};
	for i=1:length(slopeLims)
		conf.reconstr = Reconstr.SlopeLimiter(slopeLims{i});
	
		% Run solver
		soln = runSolver(conf);
		[t, U] = soln.getFinal();

		% Display data
		Plot.plotFrame(soln, U, t, 0, fig, '', styles{i});
	end
	
	% Finalize plots
	legend({'Exact', names{:}}, 'Location', 'SouthWest');
	title('');
	ylim([0,1.01]);
	Plot.makeNice();
	Plot.saveFig(['burgers_reconstr']);
	xlim([0.3, 0.7]);
	Plot.saveFig(['burgers_reconstr_closeup']);
	
	
	%%% Comparison between Godunov and Rusanov
	conf.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_MM);
	soln = runSolver(conf);
	[t, uGod] = soln.getFinal();
	
	conf.solver = Flux.Rusanov;
	soln = runSolver(conf);
	[t, uRus] = soln.getFinal();
	
	fig = figure();
	plot(xDisc, uDisc, 'k-');
	hold on;
	Plot.plotFrame(soln, uGod, t, 0, fig, '', styles{1});
	Plot.plotFrame(soln, uRus, t, 0, fig, '', styles{2});
	ylim([0, 1.01]);
	legend({'Exact', 'Godunov', 'Rusanov'}, 'Location', 'SouthWest');
	title('');
	Plot.makeNice();
	Plot.saveFig(['burgers_reconstr_compare']);
	xlim([0.3, 0.7]);
	Plot.saveFig(['burgers_reconstr_compare_closeup']);
end