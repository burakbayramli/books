function [x,stats] = dikin_solver_ineqlp(c,A,b,x0,longstep)
% uses Dikin's method to solve problems of the form
%   minimize    c^T x
%   subject to  Ax <= b
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

MAXITER = 500;
TOL = 1e-4;
stats = [];
x = x0;

for k = 1:MAXITER
    H = A'*diag((b-A*x).^(-2))*A;
    
    s = -H\c;
    lambda = diag((b-A*x).^(-2))*A*s;
    
    if longstep
        t = (b-A*x)./(A*s);
        t = min(t(t>=0));
        x = x+ .95*t*s;
    else
        x = x + s/sqrt(-c'*s);
    end
    
    stats(end+1,:) = [pos(-min(lambda)), abs(c'*x + b'*lambda)];
    if all(stats(end,:) <= TOL)
        break;
    end
end

end