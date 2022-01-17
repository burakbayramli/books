function waveBump
	
	function ret = initial(x)
		x = Utility.periodic(x, [-1, 1]);
		u = ((x>=0.25) & (x<=0.75)) .* (1-cos(2*pi*(x-0.25)).^2);
		v = zeros(size(x));
		ret = [u; v; v];
	end


	varname = 1;
	model = Model.Wave;
	conf = { Configuration(), Configuration(), Configuration() };
	conf{1}.solver = Flux.Rusanov;
	conf{2}.solver = Flux.LaxFr;
	conf{3}.solver = Flux.Wave.Godunov;
	
	
	% Iterate over each solver and run the simulation
 	soln = cell(1,3);
	for s = 1:3
		conf{s}.verbose = false;
		conf{s}.model = model;

		conf{s}.timeInt = @TimeIntegration.FE;
		conf{s}.tMax = 2;
		conf{s}.tInclude = [0.5, 1];
		conf{s}.CFL = 0.9;
		conf{s}.initial = @initial;

		conf{s}.bc = Mesh.BC.Periodic;
		conf{s}.mesh = Mesh.Cartesian([-1,1], 100);
		
		soln{s} = runSolver(conf{s});
	end
	
	
	% Get exact solution
	exact = Error.exact_wave(@initial);


	T = [0.5, 1, 2];
	style = { {'r', 'LineWidth', 2'}, ...
		{'b', 'LineWidth', 2}, ...
		{'g', 'LineWidth', 2}, ...
		{'k', 'LineWidth', 2} };
	zLim = { [0, 0.5], [0, 1], [0, 1] };
	for time = 1:3
		fig = figure();
		% Plot approximate solutions
		for s = 1:3
			[t, U] = soln{s}.getAtTime(T(time));
			u = model.getVariable(soln{s}, U, varname);
			Plot.plotFrame(soln{s}, u, t, zLim{time}, fig, '', style{s}{:});
			hold on;
		end
		
		% Plot exact solution
		mesh = soln{1}.mesh;
		uExact = exact(mesh, t, false);
		uExact = model.getVariable(soln{1}, uExact, varname);
		Plot.plotFrame(soln{1}, uExact, t, zLim{time}, fig, '', style{s+1}{:});
		
		% Make the plot nice
		Plot.makeNice;
		legend({'Rusanov', 'Lax-Friedrichs', 'Godunov', 'Exact'}, 'Location', 'Best');
		xlabel('x');
		ylabel(varname);
		title('');
		
		% Save to disk
		filename = sprintf('wave_smooth_t=%.1f', t);
		Plot.saveFig(filename);
	end
end

