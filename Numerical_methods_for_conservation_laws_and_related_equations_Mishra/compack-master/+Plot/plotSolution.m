function plotSolution( soln, ulim, varname, f, plotDiff )
%PLOTSOLUTION   Plots the computed solution
%   PLOTSOLUTION(soln) plots the (first component of) the solution 'soln',
%   using the plotting style specified by PLOT.PLOTSTYLE. The axes are
%   scaled using default MATLAB values.
%
%   PLOTSOLUTION(soln, ulim) is the same as PLOTSOLUTION(soln), but with a
%   specific axis scaling. If ulim=0, then MATLAB's default axis scaling
%   will be used (this is the default value). If ulim=-1, then MATLAB's
%   default axis scaling will be used for the first timestep, and then held
%   constant throughout the simulation. If ulim is a 2-vector, then the
%   fixed range [ulim(1), ulim(2)] will be used to scale the axes.
%
%   PLOTSOLUTION(soln, ulim, varname) plots a specific variable of the
%   solution vector. Can be any of the variable names implemented in
%   soln.getVariable(), or any integer between 1 and nelem, the number of
%   elements in the solution vector. The default value of varname is 1.
%
%   PLOTSOLUTION(soln, ulim, varname, f, true) plots the difference between
%   the computed solution and the function f=f(t,x) (or f(t,x,y)). The
%   function handle 'f' must accept the same arguments as the 'exact'
%   argument to ERROR.CALCOOC.
%
%   PLOTSOLUTION(soln, ulim, varname, f, false) attempts to plot both the
%   computed solution and f(t,x) (or f(t,x,y)) at the same time.


	narginchk(1, 5);

	% Set default values	
	if nargin <= 1
		ulim = 0;
	end
	fixAxis = ulim == -1;
	if nargin <= 2
		% Plot the first component by default
		varname = 1;
	end	
	if nargin < 5
		plotDiff = true;
	elseif plotDiff == false && soln.mesh.ndims > 1
		error('Plotting several graphs together in 2D is not supported.');
	end
	
	
	% Run through time and plot each frame	
	model = soln.config.model;
	fig = figure();	
	for itr = 1 : soln.end()
		[t, U] = soln.get(itr);		
		u = model.getVariable(soln, U, varname);
		if nargin >= 4
			V = soln.mesh.evalFunc(@(varargin) f(t,varargin{:}), soln.config.useCellAvg);
			v = model.getVariable(soln, V, varname);
			if plotDiff
				% Plot the difference between 'u' and f(x,y,t)
				Plot.plotFrame(soln, u-v, t, ulim, fig, varname);
			else
				% Plot both 'u' and f(x,y,t)
				Plot.plotFrame(soln, u, t, ulim, fig, varname);
				hold on;
				plot(soln.mesh.x, v, 'r-');
				hold off;
			end
		else
			% Plot 'u'
			Plot.plotFrame(soln, u, t, ulim, fig, varname);
		end

		% Halt at the first frame
		if itr == 1
			display('Press any key to continue...');
			waitforbuttonpress();
			if fixAxis
				ulim = ylim();
			end
		end
		
		drawnow;
	end
end
