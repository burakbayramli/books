function plotMeasure( soln, p, varname, relative, difference )
%PLOTMEASURE   Plots the L^p norm of 'varName' over time.
%
%   Arguments:
%   soln:      Solution object
%   p:         Which L^p norm to compute. Valid range is p \in [1, inf].
%   varname:   (Optional) Name of variable to plot (e.g. 'u' or 'energy').
%              Default value is the empty string ''.
%   relative:  (Optional) If true, then the relative norm
%                 (|u(t)|-|u(0)|) / |u(0)|
%              will be plotted. Default value is false.
%   difference: (Optional) If > 0, then the n-th derivative (specifically, the n-th 
% 			   divided difference) of the measurement is plotted. 
%              Default value is 0.


	% Set default values	

	error(nargchk(2, 5, nargin));

	if nargin < 3
		varname = '';
	end
	if nargin < 4
		relative = false;
	end
	if nargin < 5
		difference = 0;
	end
	
	
	% Run through time and compute the measure at each time step

	t = zeros(soln.numSoln, 1);
	measure = zeros(soln.numSoln, 1);
	for itr = 1 : soln.end()
		[t(itr), U] = soln.get(itr);
		u = soln.getVariable(U, varname);
		if relative
			if itr == 1
				u0Norm = Error.calcNorm(soln.mesh, u, p);
			end
			measure(itr) = (Error.calcNorm(soln.mesh, u, p) - u0Norm) / u0Norm;
		else
			measure(itr) = Error.calcNorm(soln.mesh, u, p);
		end
	end
	
	
	% Plot the result
	
	axisText = sprintf('||%s||_{L^{%i}}', varname, p);
	figure();
	if difference == 0
	 	plot(t, measure);
	else
		plot(t, [zeros(difference, 1); diff(measure, difference) ./ diff(t, difference)]);
		axisText = sprintf('\\partial_t^{%i} %s', difference, axisText);
	end
	title(sprintf('%s with %s on %i grid points', ...
		axisText, soln.config.solver.name, soln.mesh.nx(1)));
	xlabel('Time');
	ylabel(axisText);
end
