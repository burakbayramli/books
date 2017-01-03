function demoAdaptSimp(tol)
% demoAdaptSimp  Integrate humps(x) with adaptive Simpson's rule
%
% Synopsis:  demoAdaptSimp
%            demoAdaptSimp(tol)
%
% Input:  tol = (optional) absolute tolerance on truncation error in
%               evaluation of the integral.  Default:  tol = 5e-3
%
% Output: Value of numerical approximation to the integral and the error;
%         Min and max spacing of points along x used in evaluating integrand
if nargin<1,  tol=5e-3;  end
a = 0;  b = 2;  Iexact = humpInt(a,b);

flops(0);   [s,x] = adaptSimpsonTrace('humps',a,b,tol);  f = flops;
dx = diff(x);
fprintf('\n\tMinimum and maximum spacing     = %g    %g\n',min(dx),max(dx));
fprintf('\tExact value of the integral     = %g\n',Iexact);
fprintf('\tNumerical value of the integral = %g\n',s);
fprintf('\tError (I - Iexact)              = %g\n',s-Iexact);
fprintf('\tFlops                           = %g\n',flops);

subplot(2,1,1);   plot(x,humps(x),'o');    ylabel('y = humps(x)');
subplot(2,1,2);   plot(x(1:end-1),abs(diff(x)),'o');
xlabel('x');      ylabel('Space between f(x) evaluations');
