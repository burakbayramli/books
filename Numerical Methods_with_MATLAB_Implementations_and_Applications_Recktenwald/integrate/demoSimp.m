function demoSimp
% demoSimp  Use composite Simpson's rule to integrate x*exp(-x) on [0,5]
%
% Synopsis:  demoSimp
%
% Input:     none
%
% Output:    Table of integral values for increasing number of intervals

a = 0;  b = 5;   Iexact = -exp(-b)*(1+b) + exp(-a)*(1+a);
fprintf('\n\tIexact = %14.9f\n',Iexact);

fprintf('\n    n      h          I             error         alpha\n');
errold = [];
for np = [ 1 2 4 6 8 32 64 128]
  I = simpson('xemx',a,b,np);
  err = I - Iexact;
  n = 2*np + 1;   h = (b-a)/(n-1);     %  number of nodes and stepsize
  fprintf(' %4d %9.5f %14.9f %14.9f',n,h,I,err);

  if ~isempty(errold)
    fprintf('  %9.5f\n',log(err/errold)/log(h/hhold));
  else
    fprintf('\n');
  end
  hhold = h;  errold = err;           %  prep for next stepsize
end
