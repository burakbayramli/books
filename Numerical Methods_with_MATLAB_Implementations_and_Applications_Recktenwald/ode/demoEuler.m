function demoEuler(h)
% demoEuler  Integrate dy/dt = t - 2*y;  y(0) = 1 with Euler's method
%
% Synopsis:  demoEuler(h)
%
% Input:     h  = (optional) stepsize, Default:  h = 0.2 
%
% Output:    A table comparing the numerical and exact solutions

if nargin<1,  h = 0.2;  end

tn = 1;  y0 = 1;                      %  stopping time and IC
[t,y] = odeEuler('rhs1',tn,h,1);      %  Euler integration
yex = ( 2*t - 1 + 5*exp(-2*t) )/4;    %  Exact solution

fprintf('      t      y_Euler    y_exact     error\n');
for k=1:length(t)
  fprintf('%9.4f  %9.6f  %9.6f  %10.2e\n',t(k),y(k),yex(k),y(k)-yex(k))
end
fprintf('\nMax error = %10.2e for h = %f\n',norm(y-yex,inf),h); 
