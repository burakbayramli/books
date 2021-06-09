function [root, yval,niter] = secant(varfun,x0, x1, tol, nmax)
% input variables: varfun, x0, x1  tol, nmax
% output variables:  root, yval, niter
% varfun = the string representing a mathematical function (built-in, 
% M-file, or inline) , x0  and x1 = the two (different) initial approx.  
% The program will perform the Secant method to approximate a root of varfun 
% near x=x0 until either successive approximations differ by less than tol or 
% nmax iterations have been completed, whichever comes first.  If the tol and 
% nmax variables are omitted default values of eps*max(abs(a),abs(b),1) and 30 are used.

% we assign the default tolerance and maximum number of iterations if none
% are specified
if nargin < 4
   tol=eps*max([abs(a) abs(b) 1]); nmax=50;
end


%we now initialize the iteration
xn=x0;xnnext=x1;

%finally we set up a loop to perform the approximations
for n=1:nmax
   yn=feval(varfun, xn); ynnext=feval(varfun, xnnext);
   if ynnext == 0      
      fprintf('Exact root found\r')      
      root = xnnext; yval = 0; niter=n;      
      return    
   end
   if yn == ynnext
      error('horizontal secant encountered, Secant method failed, try changing x0, x1')
   end
   newx=xnnext-feval(varfun, xnnext)*(xnnext-xn)/(feval(varfun,xnnext)-feval(varfun,xn));
   if abs(newx-xnnext)<tol
         fprintf('The secant method has converged\r')
         root = newx; yval = feval(varfun, root); niter=n;
         return
   elseif n==nmax
         fprintf('Maximum number of iterations reached\r')
         root = newx; yval = feval(varfun, root); niter=nmax
         return
   end
   xn=xnnext; xnnext=newx;
end
   
      
         
 


   
   
  