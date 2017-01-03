function r = bisect(fun,xb,xtol,ftol,verbose)
% bisect  Use bisection to find a root of the scalar equation f(x) = 0
%
% Synopsis:  r = bisect(fun,xb)
%            r = bisect(fun,xb,xtol)
%            r = bisect(fun,xb,xtol,ftol)
%            r = bisect(fun,xb,xtol,ftol,verbose)
%
% Input: fun     = (string) name of function for which roots are sought
%        xb      = vector of bracket endpoints. xleft = xb(1), xright = xb(2)
%        xtol    = (optional) relative x tolerance.    Default:  xtol=5*eps
%        ftol    = (optional) relative f(x) tolerance. Default:  ftol=5*eps
%        verbose = (optional) print switch. Default: verbose=0, no printing
%
% Output:  r = root (or singularity) of the function in xb(1) <= x <= xb(2)
if size(xb,1)>1, warning('Only first row of xb is used as bracket');  end
if nargin < 3,  xtol = 5*eps;  end
if nargin < 4,  ftol = 5*eps;  end
if nargin < 5,  verbose = 0;   end

xeps = max(xtol,5*eps);         %  Smallest tolerances are 5*eps
feps = max(ftol,5*eps);
a = xb(1,1); b = xb(1,2);       %  Use first row if xb is a matrix
xref = abs(b - a);              %  Use initial bracket in convergence test
fa = feval(fun,a);   fb = feval(fun,b);
fref = max([abs(fa) abs(fb)]);  %  Use max f in convergence test
if sign(fa)==sign(fb)           %  Verify sign change in the interval
  error(sprintf('Root not bracketed by [%f, %f]',a,b));
end

if verbose
  fprintf('\nBisection iterations for %s.m\n',fun);
  fprintf('   k       xm            fm\n');
end
k = 0;  maxit = 50;      %  Current and max number of iterations
while k < maxit
  k = k + 1;
  dx = b - a;
  xm = a + 0.5*dx;       %  Minimize roundoff in computing the midpoint
  fm = feval(fun,xm);
  if verbose, fprintf('%4d  %12.4e  %12.4e\n',k,xm,fm); end

  if (abs(fm)/fref < feps) | (abs(dx)/xref < xeps) % True when root is found
    r = xm;  return;
  end
  if sign(fm)==sign(fa)
    a = xm;  fa = fm;    %  Root lies in interval [xm,b], replace a and fa
  else
    b = xm;  fb = fm;    %  Root lies in interval [a,xm], replace b and fb
  end
end
warning(sprintf('root not within tolerance after %d iterations\n',k));
