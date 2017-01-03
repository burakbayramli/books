function [tz,uz] = numode(method,f,t0,u0,tf,h,c)
%
%  [tz,uz] = numode(method,f,t0,u0,tf,h,c)
%    
%  General one step numerical first order ODE solver.
%    Plots components of solution in Figure #1
%    In 2-D case, also plots phase plane trajectory in Figure #2
%
% Input arguments:
%
%   method - string giving name of the function for the chosen method:  
%             'neulerf', 'ieulerf', 'midpf', 'rk4f'
%   f  - string name of function on right hand side of ODE: u' = f(t,u)
%   t0 - initial time
%   u0 - initial data u(t0)
%   tf - final time or, if vector, list of times to evaluate solution.
%   h  - step size
%
% Optional arguments:
%
%   c  - color for plot
%
% Output:
%
%   tz - times for evaluation of function as specified by tf, or, 
%        if tf is a scalar, all mesh points
%   uz - value of solution at times tz.
%
% See also NEULER, MIDP, IEULER, RK4, NEULERF, IEULERF, MIDPF, RK4F
%

l = length(tf); m = length(u0);
t1 = tf(l);

t = t0; u = u0; ts = t; us = u;

i = 0; j = 0;

while i < l
	u = feval(method,f,t,u,h); 
	t = t + h;
	j = j+1; 	ts(:,j) = t; us(:,j) = u;
	if t > tf(i+1) - h/2, i=i+1; tz(:,i) = t; uz(:,i) = u; end
end

if l==1, tz = ts; uz = us; end

figure(1);
if nargin >= 6, plot(ts,us,c); 
      if m == 2, figure(2); plot(us(1,:),us(2,:),c); axis equal; figure(1); end
   else plot(ts,us);
      if m == 2, figure(2); plot(us(1,:),us(2,:)); axis equal; figure(1); end
   end
