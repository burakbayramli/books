function linEuler_2nd
	
	function ret = initial(x)
		rho = 1 - 0.8*(x>0);
		u = zeros(size(x));
		v = zeros(size(x));
		p = rho;
		ret = [rho; u; v; p];
	end


	varname = { 'rho', 'u', 'p' };
	model = Model.LinEuler;
	conf = { Configuration(), Configuration(), Configuration() };
	ns = length(conf);
	conf{1}.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_MM);
	conf{2}.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_MC);
	conf{3}.reconstr = Reconstr.SlopeLimiter(Reconstr.Lim_SB);
	
	
	% Iterate over each solver and run the simulation
 	soln = cell(1,ns);
	for s = 1:ns
		conf{s}.model = model;
		conf{s}.solver = Flux.LinEuler.Roe;
		conf{s}.verbose = false;
		conf{s}.timeInt = @TimeIntegration.RK2;
		conf{s}.tMax = 0.5;
		conf{s}.CFL = 0.4;	
		conf{s}.initial = @initial;	
		conf{s}.bc = Mesh.BC.Neumann;
		conf{s}.mesh = Mesh.Cartesian([-1,1], 100);

		soln{s} = runSolver(conf{s});
	end
	
	
	% Get exact solution
% 	exact = Error.exact_wave(@initial);


 	zLim = { [0.18,1.02], [-0.01, 0.36], [0.18,1.02] };
	style = { {'r', 'LineWidth', 2'}, ...
		{'b', 'LineWidth', 2}, ...
		{'g', 'LineWidth', 2}, ...
		{'k', 'LineWidth', 2} };
	for var = 1:ns
		fig = figure();
		% Plot approximate solutions
		for s = 1:3
			[t, U] = soln{s}.getFinal();
			u = model.getVariable(soln{s}, U, varname{var});
			Plot.plotFrame(soln{s}, u, t, zLim{var}, fig, varname{var}, style{s}{:});
			hold on;
		end
		
% 		% Plot exact solution
% 		mesh = soln{1}.mesh;
% 		uExact = exact(mesh, t, false);
% 		uExact = model.getVariable(soln{1}, uExact, varname);
% 		Plot.plotFrame(soln{1}, uExact, t, 0, fig, '', style{s+1}{:});
		
		% Make the plot nice
		Plot.makeNice();
		legend({'Minmod', 'MC', 'Superbee'}, 'Location', 'Best');
		xlabel('x');
		title('');
		
		% Save to disk
		filename = sprintf('linEuler_2nd_%s', varname{var});
		Plot.saveFig(filename);
	end
end

