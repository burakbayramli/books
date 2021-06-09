function x = demoSSub(maxit,x0)
% demoSSub  Solve a 2-by-2 nonlinear system by successive substitution
%           The system is
%                     1.4*x1 - x2 = 0.6
%              x1^2 - 1.6*x1 - x2 = 4.6
%
% Synopsis:  x = demoSSub(maxit,x0)
%
% Input:   maxit = (optional) max number of iterations.  Default: maxit = 5
%          x0 = (optional) initial guess at solution.  Default:  x0 = [0; 0]
%
% Output:  x = estimate of solution after maxit iterations

if nargin<1,  maxit=5;           end
if nargin<2,  x0 = zeros(2,1);   end

% --- Coefficients for the case of two distinct solutions
alpha =  1.4;  bbeta = -0.6;  sigma = -1.6;  tau = -4.6;

b = [-bbeta; -tau];
x = x0;

fprintf('\n   k       x(1)        x(2)       norm(f)\n');
for k = 1:maxit
   A = [ alpha -1; (x(1)+sigma) -1];
   f = A*x - b;
   x = A\b;
   fprintf('%4d   %9.5f   %9.5f   %10.2e\n',k,x(1),x(2),norm(f));
end
