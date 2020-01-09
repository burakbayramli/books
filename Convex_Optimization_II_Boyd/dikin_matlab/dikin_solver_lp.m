function [x,stats] = dikin_solver_lp(c,A,b,x0,long_step)
% uses Dikin's method to solve problems of the form
%   minimize    c^T x
%   subject to  Ax == b
%               x >= 0
% 
% input:
%   x0: starting point. should be primal feasible
%   long_step: if true, solver takes long Dikin steps, for faster convergence
%
% output:
%   x: solution
%   stats: history of residuals for each iteration
%
% EE364b Convex Optimization II, Stephen Boyd
% written by AJ Friend 4/8/2014

TOL = 1e-4; % stopping tolerance
MAXITER = 500;
[~,n] = size(A);

x = x0;
stats = [];
for i=1:MAXITER
    AHinv = A*spdiags(x.^2,0,n,n);

    nu = (A*AHinv')\(-AHinv*c);
    s = -c.*(x.^2) - AHinv'*nu;

    if long_step
        x = x + 0.95*s/max(-s./x); 
    else
        x = x + s/norm(s./x);
    end
    
    stats(end+1,:) = [pos(-min(c+A'*nu)), abs(c'*x + b'*nu)];
    if all(stats(end,:) <= TOL)
        break;
    end
end
    
end