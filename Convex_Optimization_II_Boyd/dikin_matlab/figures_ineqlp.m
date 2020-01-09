% Dikin's method: inequality form LP examples
% produces figures for Dikin method slides
% 
% EE364b Convex Optimization II, Stephen Boyd
% written by AJ Friend 4/8/2014
%

rng(0);
m = 1000;
n = 500;

x = rand(n,1);
A = rand(m,n);
b = A*x;
A = [A;-eye(n)];
b = [b;zeros(n,1)];
c = randn(n,1);

% CVX reference solution
cvx_begin
    variable xstar(n)
    minimize( c'*xstar )
    A*xstar <= b
cvx_end

% normalize problem so |c'*xstar| = 1
c = c/abs(c'*xstar);

% CVX provides a feasible point
cvx_begin
    variable x0(n)
    A*x0 <= b
cvx_end

[x,stats] = dikin_solver_ineqlp(c,A,b,x0,true);

figure
set(gca, 'FontSize',18);
semilogy(stats(:,1:2))
h = legend('$\mbox{pos}~(-\min~(\lambda))$','$|c^T x+b^T\lambda|$');
set(h, 'interpreter', 'latex')
print -depsc ineq_lp_example
