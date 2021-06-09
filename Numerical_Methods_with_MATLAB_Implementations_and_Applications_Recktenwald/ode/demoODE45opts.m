function demoODE45opts(rtol,atol,nref)
% demoODE45opts  Integrate dy/dx = -y;  y(0) = 1 with ode45 and options
%
% Synopsis:  demoODE45opts
%            demoODE45opts(rtol)
%            demoODE45opts(rtol,atol)
%            demoODE45opts(rtol,atol,nref)
%
% Input: rtol = (optional) relative tolerance used by ode45
%               Default: rtol = 1e-3 (internal default used by ode45)
%        atol = (optional) absolute tolerance used by ode45
%               Default: atol = 1e-6 (internal default used by ode45)
%        nref = (optional) ratio of the number of solution steps returned
%               by ode45 to those actually computed with the RK-45 method.
%               Default: nref = 1, meaning all returned values are from
%               steps of the RK-45 algorithm.  nref>1 causes ode45 to
%               interpolate nref additional "solution" values per step
%
% Output:    A table comparing the numerical and exact solutions

if nargin<1, rtol = 1e-3;  end
if nargin<2, atol = 1e-6;  end
if nargin<3, nref = 1;     end

%  Set tolerance and output refinement options.
options = odeset('RelTol',rtol,'AbsTol',atol,'Refine',nref);
tn = 1;  y0 = 1;
flops(0);  [t,y] = ode45('rhs2',tn,y0,options);  f = flops;
yex = y0*exp(-t);   emax = norm(y-yex,inf);

fprintf('      t     y_ode45   y_exact    error\n');
for k=1:length(t)
  fprintf('  %7.4f  %8.6f  %8.6f  %9.2e\n',t(k),y(k),yex(k),y(k)-yex(k));
end
fprintf('\nMax error =%12.4e rtol = %9.2e atol = %9.2e\n',emax,rtol,atol);
fprintf('%g flops\n',f);
plot(t,yex,'-',t,y,'o');
title(sprintf('Solution to dy/dt = -y;  Refine = %d',nref));
xlabel('t');  ylabel('y');
