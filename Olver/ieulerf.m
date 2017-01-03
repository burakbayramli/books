function w = ieulerf(f,t,u,h)
%
%  v = ieulerf(f,t,u,h)
%    
%  Computes one step of Improved Euler method for solving a first order ODE
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
% See also NUMODE, NEULER, MIDP, IEULER, RK4, NEULERF, MIDPF, RK4F
%

g = feval(f,t,u);
v = u + h*g;
w = u + h*(g + feval(f,t+h,v))/2; 
