function x = newtonSys(Jfun,x0,xtol,ftol,maxit,verbose,varargin)
% newtonSys  Newton's method for systems of nonlinear equations.
%
% Synopsis:  x = newtonSys(Jfun,x0)
%            x = newtonSys(Jfun,x0,xtol)
%            x = newtonSys(Jfun,x0,xtol,ftol)
%            x = newtonSys(Jfun,x0,xtol,ftol,verbose)
%            x = newtonSys(Jfun,x0,xtol,ftol,verbose,arg1,arg2,...)
%
% Input:  Jfun = (string) name of mfile that returns matrix J and vector f
%         x0   = initial guess at solution vector, x
%         xtol = (optional) tolerance on norm(dx).  Default: xtol=5e-5
%         ftol = (optional) tolerance on norm(f).   Default: ftol=5e-5
%         verbose = (optional) flag.  Default: verbose=0, no printing.
%         arg1,arg2,... = (optional) optional arguments that are passed
%                   through to the mfile defined by the 'Jfun' argument
%
% Note:  Use [] to request default value of an optional input.  For example,
%        x = newtonSys('JFun',x0,[],[],[],arg1,arg2) passes arg1 and arg2 to
%        'JFun', while using the default values for xtol, ftol, and verbose
%
% Output:  x = solution vector;  x is returned after k iterations if tolerances
%              are met, or after maxit iterations if tolerances are not met.

if nargin < 3 | isempty(xtol),    xtol = 5e-5;  end
if nargin < 4 | isempty(ftol),    ftol = 5e-5;  end
if nargin < 5 | isempty(maxit),   maxit = 15;   end
if nargin < 5 | isempty(verbose), verbose = 0;  end
xeps = max(xtol,5*eps);  feps = max(ftol,5*eps);    %  Smallest tols are 5*eps

if verbose, fprintf('\nNewton iterations\n  k     norm(f)      norm(dx)\n'); end

x = x0;  k = 0;        %  Initial guess and current number of iterations
while k <= maxit
  k = k + 1;
  [J,f] = feval(Jfun,x,varargin{:});   %   Returns Jacobian matrix and f vector
  dx = J\f;
  x = x - dx;
  if verbose, fprintf('%3d %12.3e %12.3e\n',k,norm(f),norm(dx));  end
  if ( norm(f) < feps ) | ( norm(dx) < xeps ),  return;   end
end
warning(sprintf('Solution not found within tolerance after %d iterations\n',k));
