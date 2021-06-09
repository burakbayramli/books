function demoODE45
% demoODE45  Integrate dy/dx = -y;  y(0) = 1 with ode45
%
% Synopsis:  demoODE45
%
% Input      none
%
% Output:    Table and plot comparing the numerical and exact solutions

tn = 1;  y0 = 1;                %  Length of interval and IC
[t,y] = ode45('rhs2',tn,y0);    %  ode45 solution
yex = y0*exp(-t);               %  Exact solution
fprintf('      t     y_ode45   y_exact      error\n');
for k=1:length(t)
  fprintf('  %7.4f  %8.6f  %8.6f  %11.2e\n',t(k),y(k),yex(k),y(k)-yex(k));
end
fprintf('\nMax error = %12.4e\n',norm(y-yex,inf)); 
plot(t,yex,'-',t,y,'o');  xlabel('t');  ylabel('y');
