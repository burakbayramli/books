function  fidiff(x)
% fidiff  First order finite-difference approximation to d/dx of exp(x)
%
% Synopsis:  fidiff(x)
%
% Input:     x = (optional) value at which the derivative is to be evaluated
%                Default:  x = 1
%
% Output:    A table and plot of relative error versus stepsize

if nargin<1, x=1;  end
fp = exp(x);                     %  Exact value of fprime
h = logspace(-12,0,13)';         %  Column vector of stepsizes
fpfd = (exp(x+h) - exp(x))./h;   %  Vectorized calculation of fd approximation
Erel = abs(fpfd - fp)./fp;       %  and relative errors

fprintf('      h        fp       fpfd         Erel\n');
for k=1:length(h)
  fprintf('%10.1e %9.5f %9.5f %12.2e\n',h(k),fp,fpfd(k),Erel(k));
end

loglog(h,Erel,'+');  xlabel('Stepsize,  h');    ylabel('Relative error');