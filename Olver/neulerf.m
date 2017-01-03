function v = neulerf(f,t,u,h)
%
%  v = neulerf(f,t,u,h)
%    
%  Computes one step of Euler method for solving a first order ODE
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
% See also NUMODE, NEULER, MIDP, IEULER, RK4, IEULERF, MIDPF, RK4F
%

v =  u + h*feval(f,t,u); 

