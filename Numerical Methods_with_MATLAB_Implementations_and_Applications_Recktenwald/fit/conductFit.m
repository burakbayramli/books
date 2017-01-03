function conductFit(fname)
% conductFit  LS fit of conductivity data for Copper at low temperatures
%
% Synopsis:  conductFit(fname)
%
% Input:  fname   = (optional, string) name of data file;
%                   Default: fname = 'conduct1.dat'
%
% Output:  Print out of curve fit coefficients and a plot comparing data
%          with the curve fit for two sets of basis functions.

if nargin<1,  fname = 'cucon1.dat';   end   %  Default data file

% --- define basis functions as inline function objects
fun1 = inline('[1./t  t.^2]');        %  t must be a column vector
fun2 = inline('[1./t  t  t.^2]');

% --- read data and perform the fit
[t,k] = loadColData(fname,2,0,2);     %  Read data into t and k
[c1,R21,r1] = fitnorm(t,1./k,fun1);   %  Fit to first set of bases
[c2,R22,r2] = fitnorm(t,1./k,fun2);   %  and second set of bases

% --- print results
fprintf('\nCurve fit to data in %s\n\n',fname);
fprintf(' Coefficients of      Basis Fcns 1       Basis Fcns 2\n');
fprintf('      T^(-1)       %16.9e   %16.9e\n',c1(1),c2(1));
fprintf('      T            %16.9e   %16.9e\n',0,c2(2));
fprintf('      T^2          %16.9e   %16.9e\n',c1(2),c2(3));
fprintf('\n    ||r||_2        %12.5f      %12.5f\n',norm(r1),norm(r2));
fprintf('      R2           %12.5f      %12.5f\n',R21,R22);

% --- evaluate and plot the fits
tf = linspace(0.1,max(t))';     %  100 T values: 0 < t <= max(t)
Af1 = feval(fun1,tf);           %  A matrix evaluated at tf values
kf1 = 1./ (Af1*c1);             %  Af*c is column vector of 1/kf values
Af2 = feval(fun2,tf);
kf2 = 1./ (Af2*c2);
plot(t,k,'o',tf,kf1,'--',tf,kf2,'-');
xlabel('Temperature  (K)');   ylabel('Conductivity  (W/m/C)');
legend('data','basis 1','basis 2');
