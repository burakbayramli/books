function x = bisect(int, tol)
% bisect - bisection algorithm for the scalar equation f(x) = 0
% 
%   Syntax: 
%           x = bisect(int, tol)
%   Arguments: 
%           int - 2-element vector specifying an interval int = [a, b]
%           tol - a tolerance 
%   Returns: 
%           x - an approximate solution in the interval int = [a, b]
%   Description:
%           The program bisect uses the bisection algorithm to 
%           compute an approximate solution of the scalar equation
%           f(x) = 0 in the interval int = [a,b].  The file f.m must 
%           contain the function y = f(x).  The function values f(a) 
%           and f(b) must have opposite signs.  Then the program
%           computes an approximate solution x with the error
%           |x - x_exact| < tol.  It returns an empty value for x if
%           f(a) and f(b) have the same sign.  
%   Examples: 
%          Define the function f in f.m as:
%
%          function y = f(x)
%          % f(x)
%          % Returns the sine of x.
%          y = sin(x)
%
%          Then:
%
%          x = bisect([1,4], 1e-7)   computes pi to 7 decimals
%
%----------------------------------------------------------------------

a = int(1);                  % left interval endpoint 
b = int(2);                  % right endpoint

% add code here to interchange a and b in case a > b.

fa = f(a);                   % evaluate f at a
fb = f(b);                   % evaluate f at b

% add code here that displays an appropriate message, and then stops
% and returns x = [] if fa and fb do not have opposite signs

% add code here that displays an appropriate message and stops if the
% given tolerance is not positive.

% replace the ?'s to get a working bisection solver
% test the solver on problems with known solution to ensure it works
 
while (b - a > tol)

  x = ?;                     % compute midpoint x of (current) interval [a b]
  fx = f(x);                 % evaluate f at midpoint

  if fa * fx < 0             % change of sign in left half 
    b = ?;                   % make left half new current interval
  else
    ? = ?;                   % else right half new current interval 
  end 

end 

x = (a + b) / 2;
