function [F,J] = FunctionEvaluator(x)
% Evaluate the vector function and the Jacobian matrix for 
% the system of nonlinear equations derived from the general 
% n-dimensional Rosenbrock function.
% Get the problem size
n = length(x);  
if n == 0, error('Input vector, x, is empty.'); end
if mod(n,2) ~= 0, 
   error('Input vector, x ,must have an even number of components.');
end
% Evaluate the vector function
odds  = 1:2:n;
evens = 2:2:n;
F = zeros(n,1);
C = zeros(n,n); D = zeros(n,n); E=zeros(n,n);
F(odds,1)  = (1-x(odds)).^2;
F(evens,1) = 10.*((x(odds)-x(evens).^2)).^2; 
% Evaluate the Jacobian matrix if nargout > 1 
if nargout > 1
   c = 2.*(x(odds)-1);    C(sub2ind([n,n],odds,odds))=c;
   d = 40.*(x(evens).*(x(evens).^2-x(odds)));  D(sub2ind([n,n],evens,evens))=d;
   e = 20.*(x(odds)-x(evens).^2);    E(sub2ind([n,n],evens,odds))=e;
   J = C + D + E;
end

