function v = rk4f(f,t,u,h)
%
%  v = rk4f(f,t,u,h)
%    
%  Computes one step of 4th order Runge-Kutta method for solving a first order ODE
%
% Input arguments:
%
%   f  - string name of function on right hand side of ODE: u' = f(t,u)
%   t  - current time
%   u  - current value of solution
%   h  - step size
%
% Output:
%
%   v  - updated value of solution
%
% See also NUMODE, NEULER, MIDP, IEULER, RK4, NEULERF, IEULERF, MIDPF
%

f1 = feval(f,t,u);
f2 = feval(f,t+h/2,u+h*f1/2);
f3 = feval(f,t+h/2,u+h*f2/2);
f4 = feval(f,t+h,u+h*f3);
v =  u + h*(f1 + 2*f2 + 2*f3 + f4)/6; 
