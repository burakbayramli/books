function A=jacobi(f,x)
% Computes the derivative (Jacobi matrix) A=Df(x) of the function f at x.
% 
%   Syntax: 
%           A = jacobi(f,x)
%   Arguments: 
%           f - string containing the name of a function file  
%               which returns an mx1 matrix y=f(x).  
%           x - an nx1 matrix
%   Returns: 
%           A - an mxn matrix, the Jacobian of f 
%   Description:
%           The program jacobi uses a central difference quotient
%           to compute an approximation of the Jacobi matrix A=Df(x)
%           The file f.m must return an mx1 matrix y=f(x).  
%           The stepsize is h=1e-6.  
%   Examples: 
%          z = jacobi('sin', 0) returns 1.
%
%          If the m-file funk1.m contains 
%            function y = funk1(x)
%            y=x.^2-2;
%
%          then the command 
%            z = jacobi('funk1', 3) returns 6.
%
%          If the m-file funk1.m contains 
%            function y = funk2(x)
%            y=x'*x;
%
%          then the command 
%            A = jacobi('funk2', [1;2;3]) returns [2 4 6]. 
%   See also:
%

%----------------------------------------------------------------------
%   Date created: 
%
%   Author:  
%------------------------------+---------------------------------------+

h = 1e-6;
n=length(x);

for i=1:n
  x1 = x;
  x1(i) = x1(i) + h;
  x2 = x;
  x2(i) = x2(i) - h;
  A(:,i)= (feval(f,x1)-feval(f,x2))/(2*h);
end
  
