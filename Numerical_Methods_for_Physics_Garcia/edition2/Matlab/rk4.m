function xout = rk4(x,t,tau,derivsRK,param)
%  Runge-Kutta integrator (4th order)
% Input arguments -
%   x = current value of dependent variable
%   t = independent variable (usually time)
%   tau = step size (usually timestep)
%   derivsRK = right hand side of the ODE; derivsRK is the
%             name of the function which returns dx/dt
%             Calling format derivsRK(x,t,param).
%   param = extra parameters passed to derivsRK
% Output arguments -
%   xout = new value of x after a step of size tau
half_tau = 0.5*tau;
F1 = feval(derivsRK,x,t,param);  
t_half = t + half_tau;
xtemp = x + half_tau*F1;
F2 = feval(derivsRK,xtemp,t_half,param);  
xtemp = x + half_tau*F2;
F3 = feval(derivsRK,xtemp,t_half,param);
t_full = t + tau;
xtemp = x + tau*F3;
F4 = feval(derivsRK,xtemp,t_full,param);
xout = x + tau/6.*(F1 + F4 + 2.*(F2+F3));
return;
