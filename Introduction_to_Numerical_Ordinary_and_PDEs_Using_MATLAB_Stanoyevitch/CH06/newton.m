function [root, yval] = newton(varfun, dvarfun,  x0, tol, nmax)
% input variables: varfun, dvarfun, x0, tol, nmax
% output variables:  root, yval
% varfun = the string representing a mathematical function (built-in, 
% M-file, or inline) and dvarfun = the string representing the 
% derivative, x0 = the initial approx.  The program will perform 
% Newton's method to approximate a root of varfun near x=x0 until either
% successive approximations differ by less than tol or nmax iterations have 
% been completed, whichever comes first.  If the tol and nmax variables are
% omitted, default values of eps*max(abs(a),abs(b),1) and 30 are used.

% we assign the default tolerance and maximum number of iterations if none
% are specified
if nargin < 4
   tol=eps*max([abs(a) abs(b) 1]); nmax=30;
end

%we now initialize the iteration
xn=x0;

%finally we set up a loop to perform the approximations
for n=1:nmax
   yn=feval(varfun, xn); ypn=feval(dvarfun, xn);
   if yn == 0
      fprintf('Exact root found\r')
      root = xn; yval = 0;
      return 
   end
   if ypn == 0
      error('Zero derivative encountered, Newton''s method failed, try changing x0')
   end
   xnew=xn-yn/ypn;
   if abs(xnew-xn)<tol
         fprintf('Newton''s method has converged\r')
         root = xnew; yval = feval(varfun, root);
         return
   elseif n==nmax
         fprintf('Maximum number of iterations reached\r')
         root = xnew; yval = feval(varfun, root);
         return
   end
   xn=xnew;
end
