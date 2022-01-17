function linAdv_reconstr
	conf = Configuration;
	conf.model = Model.LinAdv;
	conf.model.a = 1;
	conf.timeInt = @TimeIntegration.FE;	
	conf.solver = Flux.LinAdv.Exact_2nd;
	conf.CFL = 0.8;
	
	
	
	% Exact solutions
	xDisc = linspace(0,2,1000);
	uDisc = 1 + (xDisc < 1.5);
	xSine = linspace(0,1,1000);
	uSine = sin(4*pi*xSine);
	
	% Figures comparing all reconstruction methods
	figComp = [figure(), figure()];
	
	% Plot exact solutions
	set(0, 'CurrentFigure', figComp(1));
	plot(xDisc, uDisc, 'k-');
	hold on;
	set(0, 'CurrentFigure', figComp(2));
	plot(xSine, uSine, 'k-');
	hold on;
	
	
	slopeLims = { Reconstr.Lim_MM, Reconstr.Lim_MC, Reconstr.Lim_SB };
	names = {'Minmod', 'MC', 'Superbee'};	
	styles = {'bo-', 'ro-', 'go-'};
	for i=1:length(slopeLims)
		conf.reconstr = Reconstr.SlopeLimiter(slopeLims{i});
		
		
		%%% Experiment 1 (discontinuous solution)
		conf.tMax = 1;
		conf.initial = @(x) 1 + (x<0.5);
		conf.bc = Mesh.BC.Neumann;
		conf.mesh = Mesh.Cartesian([0,2], 50);

		% Run solver
		soln = runSolver(conf);
		[t, u] = soln.getFinal();

		% Display data
		fig = figure();
		plot(xDisc, uDisc, 'k-');
		hold on;
		Plot.plotFrame(soln, u, t, 0, fig, '', 'bo-');
		legend({'Exact', names{i}}, 'Location', 'SouthWest');
		title('');
		axis tight;
		Plot.makeNice();
		Plot.saveFig(['linAdv_', names{i}, '_disc']);
		
		Plot.plotFrame(soln, u, t, 0, figComp(1), '', styles{i});



		%%% Experiment 2 (sine wave)
		conf.tMax = 1;
		conf.initial = @(x) sin(4*pi*x);
		conf.bc = Mesh.BC.Periodic;
		conf.mesh = Mesh.Cartesian([0,1], 50);

		% Run solver
		soln = runSolver(conf);
		[t, u] = soln.getFinal();

		% Display data
		fig = figure();
		plot(xSine, uSine, 'k-');
		hold on;
		Plot.plotFrame(soln, u, t, 0, fig, '', 'bo-');
		legend({'Exact', names{i}});
		title('');
		Plot.makeNice();
		Plot.saveFig(['linAdv_', names{i}, '_sine']);
		
		Plot.plotFrame(soln, u, t, 0, figComp(2), '', styles{i});
	end
	
	
	% Finalize comparison plots
	set(0, 'CurrentFigure', figComp(1));
	legend({'Exact', names{:}}, 'Location', 'SouthWest');
	title('');
	Plot.makeNice();
	Plot.saveFig(['linAdv_reconstr_disc']);
	xlim([1.3, 1.7]);
	Plot.saveFig(['linAdv_reconstr_disc_closeup']);
	
	set(0, 'CurrentFigure', figComp(2));
	legend({'Exact', names{:}});
	title('');
	Plot.makeNice();
	Plot.saveFig(['linAdv_reconstr_sine']);	
end