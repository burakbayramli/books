function linEuler
	
	function ret = initial(x)
		rho = 1 - 0.8*(x>0);
		u = zeros(size(x));
		v = zeros(size(x));
		p = rho;
		ret = [rho; u; v; p];
	end


	varname = { 'rho', 'u', 'p' };

	conf = { Configuration(), Configuration(), Configuration() };
	conf{1}.solver = Flux.Rusanov;
	conf{2}.solver = Flux.LaxFr;
	conf{3}.solver = Flux.LinEuler.Roe;
	
	
	% Iterate over each solver and run the simulation
 	soln = cell(1,3);
	for s = 1:3
		conf{s}.model = Model.LinEuler;
		conf{s}.verbose = false;

		conf{s}.timeInt = @TimeIntegration.FE;
		conf{s}.tMax = 0.5;
		conf{s}.CFL = 0.4;	
		conf{s}.initial = @initial;	
		conf{s}.bc = Mesh.BC.Neumann;
		conf{s}.mesh = Mesh.Cartesian([-1,1], 100);

		soln{s} = runSolver(conf{s});
	end
	
	
	% Get exact solution
% 	exact = Error.exact_wave(@initial);


	T = 0.5;
	style = { {'r', 'LineWidth', 2'}, ...
		{'b', 'LineWidth', 2}, ...
		{'g', 'LineWidth', 2}, ...
		{'k', 'LineWidth', 2} };
	for var = 1:3
		fig = figure();
		set(gca, 'FontSize', 12);
		% Plot approximate solutions
		for s = 1:3
			[t, U] = soln{s}.getAtTime(T);
			u = conf{s}.model.getVariable(soln{s}, U, varname{var});
			Plot.plotFrame(soln{s}, u, t, 0, fig, varname{var}, style{s}{:});
			hold on;
		end
		
% 		% Plot exact solution
% 		mesh = soln{1}.mesh;
% 		uExact = exact(mesh, t, false);
% 		uExact = getVariable(soln{1}, uExact, varname);
% 		Plot.plotFrame(soln{1}, uExact, t, 0, fig, '', style{s+1}{:});
		
		% Make the plot nice
		Plot.makeNice();
		legend({'Rusanov', 'Lax-Friedrichs', 'Godunov'}, 'Location', 'Best');
		xlabel('x');
		title('');
		
		% Save to disk		
		filename = sprintf('linEuler_%s', varname{var});
		Plot.saveFig(filename);
	end
end

