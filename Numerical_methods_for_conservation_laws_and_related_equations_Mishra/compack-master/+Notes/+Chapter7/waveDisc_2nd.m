function waveDisc_2nd
	
	function ret = initial(x)
		x = Utility.periodic(x, [-1, 1]);
		u = (x>=0.25) & (x<=0.75);
		v = zeros(size(x));
		ret = [u; v; v];
	end


	varname = 1;
	numX = 100;
	model = Model.Wave;

	conf = { Configuration(), Configuration(), Configuration() };
	ns = length(conf);
	conf{1}.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_MM);
	conf{2}.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_MC);
	conf{3}.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_SB);
	
	
	% Iterate over each solver and run the simulation
 	soln = cell(1,ns);
	for s = 1:ns
		conf{s}.verbose = false;
		conf{s}.model = model;
		conf{s}.solver = Flux.Wave.Godunov;

		conf{s}.timeInt = @TimeIntegration.RK2;
		conf{s}.tMax = 2;
		conf{s}.CFL = 0.9;
		conf{s}.initial = @initial;

		conf{s}.bc = Mesh.BC.Periodic;
		conf{s}.mesh = Mesh.Cartesian([-1,1], numX);
		
		soln{s} = runSolver(conf{s});
	end
	
	
	% Get exact solution
	exact = Error.exact_wave(@initial);


	T = 2;
	zLim = [0,1.1];
	style = { {'r', 'LineWidth', 2'}, ...
		{'b', 'LineWidth', 2'}, ...
		{'g', 'LineWidth', 2}, ...
		{'k', 'LineWidth', 2} };

	
	fig = figure();
	% Plot approximate solutions
	for s = 1:ns
		[t, U] = soln{s}.getAtTime(T);
		u = model.getVariable(soln{s}, U, varname);
		Plot.plotFrame(soln{s}, u, t, zLim, fig, '', style{s}{:});
		hold on;
	end

	% Plot exact solution
	mesh = soln{1}.mesh;
	uExact = exact(mesh, t, false);
	uExact = model.getVariable(soln{1}, uExact, varname);
	Plot.plotFrame(soln{1}, uExact, T, zLim, fig, '', style{s+1}{:});

	% Make the plot nice
	Plot.makeNice();
	legend({'Minmod', 'MC', 'Superbee', 'Exact'}, 'Location', 'Best');
	xlabel('x');
	ylabel(varname);
	title('');

	% Save to disk
	Plot.saveFig('wave_disc_2nd_comp'); 
	
	
	
	% Reconstruct in eigenspace
		
	conf{1}.reconstr = Reconstr.SlopeLimiterEigen(Reconstr.Lim_MM);
	conf{2}.reconstr = Reconstr.SlopeLimiterEigen(Reconstr.Lim_MC);
	conf{3}.reconstr = Reconstr.SlopeLimiterEigen(Reconstr.Lim_SB);
	for s = 1:ns	
		soln{s} = runSolver(conf{s});
	end
		
	fig = figure();
	% Plot approximate solutions
	for s = 1:ns
		[t, U] = soln{s}.getAtTime(T);
		u = model.getVariable(soln{s}, U, varname);
		Plot.plotFrame(soln{s}, u, t, 0, fig, '', style{s}{:});
		hold on;
	end

	% Plot exact solution
	mesh = soln{1}.mesh;
	uExact = exact(mesh, t, false);
	uExact = model.getVariable(soln{1}, uExact, varname);
	Plot.plotFrame(soln{1}, uExact, T, 0, fig, '', style{s+1}{:});

	% Make the plot nice
	Plot.makeNice();
	legend({'Minmod', 'MC', 'Superbee', 'Exact'}, 'Location', 'Best');
	xlabel('x');
	ylabel(varname);
	title('');

	% Save to disk
	Plot.saveFig('wave_disc_2nd_eigen'); 
end