function err = calcError( exact, soln, p, varname, relative )
%CALCERROR   Calculates the L^p error in an approximate solution at the
%            final time step.
%
% Arguments:
% exact:    Function taking three variables (x,t,cellAvg) and returning the 
%           exact solution at the point (x,t). If cellAvg==true then the
%           function must return the solution in the cell average sense.
% soln:     Solution object of approximate solution.
% p:        Which L^p norm to compute in
% varname:  (Optional) Name of variable to compute error in. 
%           Default is ''.
% relative: (Optional) If true then the relative error
%               |u(t)-v(t)| / |v(t)|
%           (v being the exact solution) will be computed. Default is false.


	assert(nargin >= 3, 'Not enough input arguments.');
	
	% Set default values
	if nargin < 4
		varname = '';
	end
	if nargin < 5
		relative = false;
	end
	
	% Get the approximate and exact solutions
	[t, U] = soln.get(soln.end());
	u = soln.getVariable(U, varname);
	uExact = exact(soln.mesh, t, soln.config.useCellAvg);
	uExact = uExact(:, soln.mesh.internal);
	
	% Calculate errors
	err = Error.calcNorm(soln.mesh, u-uExact, p);
	if relative
		err = err / Error.calcNorm(soln.mesh, uExact, p);
	end

end
