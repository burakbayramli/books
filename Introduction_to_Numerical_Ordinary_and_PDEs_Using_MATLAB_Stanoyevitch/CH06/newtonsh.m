function A = newtonsh(varfun, dvarfun,  x0, tol, nmax)
% input variables: varfun, dvarfun, x0,  tol, nmax
% output variables:  A
% varfun = the string representing a mathematical function (built-in, 
% M-file, or inline) and dvarfun = the string representing the 
% derivative, x0 = the initial approx.  The program will perform the 
% Newton's Method to approximate a root of varfun near x=x0 until either
% successive approximations differ by less than tol or nmax iterations have 
% been completed, whichever comes first.  If the tol and nmax variables are
% omitted default values of eps*max(abs(a),abs(b),1) and 30 are used.
% the output will be an n by 2 matrix where the first column tells the 
% successive Newton approximation and the third shows the corresponding y-values

% we assign the default tolerance and maximum number of iterations if none
% are specified
if nargin < 4
   tol=eps*max([abs(a) abs(b) 1]); nmax=30;
end


%we now initialize the iteration
xn=x0;

%finally we set up a loop to perform the approximations
for n=1:nmax
   yn=feval(varfun, xn);, ypn=feval(dvarfun, xn);
   if ypn == 0
      error('zero derivative encountered, Newton''s method failed, try changing x0')
   end
   xnew=xn-yn/ypn;
   yval=feval(varfun,xnew);
   A(n,:)=[xnew  yval];
   if abs(xnew-xn)<tol
         fprintf('Newton''s method has converged')
         return
   elseif n==nmax
         fprintf('Maximum number of iterations reached')
         return
   end
   xn=xnew;
end
   
      
         
 


   
   
  