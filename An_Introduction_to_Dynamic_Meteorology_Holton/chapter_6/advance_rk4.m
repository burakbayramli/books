%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   advance_le_glob - function that integrates the Lorenz 96 model 
%                     forward in time by a specified time interval by 
%                     using a second order Runge-Kutta scheme.
%
%   x1 = advance_le_glob(deltat, x0)
%
%    deltat - time interval to do forecast for
%        x0 - initial condition for forecast
%        x1 - forecast deltat time in the future
%
%     created May 2003 Ryan Torn, U. Washington based on code from C. Snyder, NCAR
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [T,xout] = advance_rk4(rhs_handle, dt, tspan, x0, varargin)

% varargin contains extra parameters for call to rhs_handle
%disp(['varargin in advance...' varargin])

deltat = tspan(end);
Ntims = round(deltat ./ dt);

% times to return solution
save_times = round(tspan/dt);
%disp(['saving time steps:' num2str(save_times)])

% initialize output matrix
xout = zeros(length(save_times),length(x0));
%disp(['dt = ' num2str(dt)])
%disp(['running ' int2str(Ntims) ' times'])

x1 = x0;
ic = 0;
for time = 0:Ntims-1

  t = time*dt; % use this in calls below for conformity with ode45
  disp([num2str(t) ' :time step number ' int2str(time) ' of ' int2str(Ntims)])

  if (sum(save_times == time) > 0)
	 ic = ic + 1;
	 xout(ic,:) = x1;
	 T(ic) = time;
  end
  
  q1 = dt .* rhs_handle(t,x1,varargin{:});
  q2 = dt .* rhs_handle(t,x1 + q1 ./ 2,varargin{:});
  q3 = dt .* rhs_handle(t,x1 + q2 ./ 2,varargin{:});
  q4 = dt .* rhs_handle(t,x1 + q3,varargin{:});

  x1 = x1 + (q1 + 2 .* q2 + 2 .* q3 + q4) ./ 6;

end;

% save final time
ic = ic + 1;
xout(ic,:) = x1;
