function demoRK4(h)
% demoRK4  Integrate  dy/dt = t-2*y;  y(0) = 1 with RK4 method
%
% Synopsis:  demoRK4
%            demoRK4(h)
%
% Input:     h = (optional) stepsize;  Default: h = 0.2
%
% Output:    A table comparing the numerical and exact solutions

if nargin<1, h=0.2;  end
tn = 1;  y0 = 1;      %  stopping time and initial condition

dydt = inline('t-2*y','t','y')
yexact = inline('(2*t - 1 + 5*exp(-2*t))/4')

[t,y] = odeRK4(dydt,tn,h,y0);      %  RK4 solution
yex = yexact(t);                   %  Exact solution
emax = max(abs(y-yex));            %  Largest error

fprintf('\n      t      y_RK4    y_exact    error\n');
for k=1:length(t)
   fprintf('  %7.4f  %8.6f  %8.6f  %9.2e\n',t(k),y(k),yex(k),y(k)-yex(k))
end
fprintf('\nMax error = %9.2e for h = %f\n',emax,h);
plot(t,yex,'-',t,y,'o');
