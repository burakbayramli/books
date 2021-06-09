function demoODE45args(alpha)
% demoODE45args  Integrate dy/dt = -alpha*y;  y(0) = 1 with variable alpha
%
% Synopsis:  demoODE45
%            demoODE45(alpha)
%
% Input      alpha = (optional) decay rate coefficient.  Default:  alpha = 5
%                   Default: rtol = 1e-3 (internal default used by ode45)
%
% Output:    A table and plot comparing the numerical and exact solutions

if nargin<1, alpha = 2;  end

tn = 1;  y0 = 1;
[t,y] = ode45('rhsDecay',tn,y0,[],alpha);
yex = y0*exp(-alpha*t);
fprintf('      t     y_ode45   y_exact    error\n');
for k=1:length(t)
  fprintf('  %7.4f  %8.6f  %8.6f  %9.2e\n',t(k),y(k),yex(k),y(k)-yex(k));
end
fprintf('\nMax error = %10.2e for alpha = %9.2e\n',norm(y-yex,inf),alpha);
plot(t,yex,'-',t,y,'o');
title(sprintf('Solution to dy/dt = -%g*y',alpha));  xlabel('t');  ylabel('y');
