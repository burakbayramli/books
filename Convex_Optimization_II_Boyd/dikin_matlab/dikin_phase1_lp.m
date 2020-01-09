function [x,feas,stats] = dikin_phase1_lp(A,b)
% uses Dikin's method for a Phase I procedure to find a feasible point for
%   find        x
%   subject to  Ax == b
%               x >= 0
%
% output:
%   x: feasible point, if feas is true
%   feas: true if problem is strictly feasible
%   stats: history of residuals for each iteration
%
% EE364b Convex Optimization II, Stephen Boyd
% written by AJ Friend 4/8/2014

[~,n] = size(A);

G = [A, -sum(A,2)];
h = b - sum(A,2);
c = zeros(n+1,1);
c(end) = 1;

x = A\b;
t = max(0,max(2-x));
z = x + (t-1);

[zt,stats] = dikin_solver_lp(c,G,h,[z;t],true);
feas = (zt(end) < 1);
x = zt(1:n) + 1 - zt(end);
end