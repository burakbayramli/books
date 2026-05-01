function [xSmall, t, tau] = rka(x,t,tau,err,derivsRK,param)
% Adaptive Runge-Kutta routine
% Inputs
%   x          Current value of the dependent variable
%   t          Independent variable (usually time)
%   tau        Step size (usually time step)
%   err        Desired fractional local truncation error
%   derivsRK   Right hand side of the ODE; derivsRK is the
%              name of the function which returns dx/dt
%              Calling format derivsRK(x,t,param).
%   param      Extra parameters passed to derivsRK
% Outputs
%   xSmall     New value of the dependent variable
%   t          New value of the independent variable
%   tau        Suggested step size for next call to rka

%* Set initial variables
tSave = t;  xSave = x;    % Save initial values
safe1 = .9;  safe2 = 4.;  % Safety factors

%* Loop over maximum number of attempts to satisfy error bound
maxTry = 100;  
for iTry=1:maxTry
	
  %* Take the two small time steps
  half_tau = 0.5 * tau;
  xTemp = rk4(xSave,tSave,half_tau,derivsRK,param);
  t = tSave + half_tau;
  xSmall = rk4(xTemp,t,half_tau,derivsRK,param);
  
  %* Take the single big time step
  t = tSave + tau;
  xBig = rk4(xSave,tSave,tau,derivsRK,param);
  
  %* Compute the estimated truncation error
  scale = err * (abs(xSmall) + abs(xBig))/2.;
  xDiff = xSmall - xBig;
  errorRatio = max( abs(xDiff)./(scale + eps) );
  
  %* Estimate new tau value (including safety factors)
  tau_old = tau;
  tau = safe1*tau_old*errorRatio^(-0.20);
  tau = max(tau,tau_old/safe2);
  tau = min(tau,safe2*tau_old);
  
  %* If error is acceptable, return computed values
  if (errorRatio < 1)  return;  end 
end

%* Issue error message if error bound never satisfied
error('ERROR: Adaptive Runge-Kutta routine failed');
return;
  
