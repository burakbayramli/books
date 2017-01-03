function x = newton(fun,x0,xtol,ftol,verbose,varargin)
% newtonNG   Newton's method to find a root of the scalar equation f(x) = 0,
%            no global (NG) variables are needed to pass 
%
% Synopsis:  x = newton(fun,x0)
%            x = newton(fun,x0,xtol)
%            x = newton(fun,x0,xtol,ftol)
%            x = newton(fun,x0,xtol,ftol,verbose)
%            x = newton(fun,x0,xtol,ftol,verbose,opt1,opt2,...)
%
% Input:  fun     = (string) name of mfile that returns f(x) and f'(x).
%         x0      = initial guess
%         xtol    = (optional) absolute tolerance on x
%         ftol    = (optional) absolute tolerance on f(x).
%         verbose = (optional) flag.  Default: verbose=0, no printing.
%         opt1,opt2,... = (optional) optional arguments that are passed
%                   through to the mfile defined by the 'fun' argument
%
%  Output:  x = the root of the function

if nargin < 3 | isempty(xtol), xtol = 5*eps;  end
if nargin < 4 | isempty(ftol), ftol = 5*eps;  end
if nargin < 5, verbose = 0;  end
xeps = max(xtol,5*eps);  feps = max(ftol,5*eps);  %  Smallest tols are 5*eps

if verbose
  fprintf('\nNewton iterations for %s.m\n',fun);
  fprintf('  k      f(x)         dfdx         x(k+1)\n');
end

x = x0;  k = 0;  maxit = 15;    %  Initial guess, current and max iterations
while k <= maxit
  k = k + 1;
  [f,dfdx] = feval(fun,x,varargin{:});   %   Returns f( x(k-1) ) and f'( x(k-1) )
  dx = f/dfdx;
  x = x - dx;
  if verbose, fprintf('%3d %12.3e %12.3e %18.14f\n',k,f,dfdx,x);  end

  if ( abs(f) < feps ) | ( abs(dx) < xeps ),  return;  end
end
warning(sprintf('root not found within tolerance after %d iterations\n',k));
