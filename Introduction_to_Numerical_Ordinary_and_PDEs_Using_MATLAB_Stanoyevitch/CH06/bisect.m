function [root, yval] = bisect(varfun, a, b, tol)
% input variables: varfun, a, b, tol
% output variables:  root, yval
% varfun = the string representing a mathematical function (built-in, 
% M-file, or inline) of one variable that is assumed to have opposite 
% signs at the points x=a,  and x=b.  The program will perform the 
% bisection method to approximate a root of varfun in [a,b] with an 
% error < tol.  If the tol variable is omitted a default value of 
% eps*max(abs(a),abs(b),1)is used.

%we first check to see if there is the needed sign change
ya=feval(varfun,a); yb=feval(varfun,b);
if sign(ya)==sign(yb)
   error('function has same sign at endpoints')
end

%we assign the default tolerance, if none is specified
if nargin < 4
   tol=eps*max([abs(a) abs(b) 1]);
end


%we now initialize the iteration
an=a; bn=b; n=0;

%finally we set up a loop to perform the bisections
while (b-a)/2^n >= tol
   xn=(an + bn)/2; yn=feval(varfun, xn); n=n+1
   if yn==0
      fprintf('numerically exact root')
      root=xn; yval=yn;
      return
   elseif sign(yn)==sign(ya)
      an=xn; ya=yn;
   else
      bn=xn; yb=yn;
   end
end

root=xn; yval=yn;
