% Dikin's method: LP examples
% produces figures for Dikin method slides
% 
% EE364b Convex Optimization II, Stephen Boyd
% written by AJ Friend 4/8/2014
%

rng(0);

% smaller example with short step
m = 100;
n = 400;
A = randn(m,n);
b = randn(m,1);
c = rand(n,1);

[x0,feas,stats] = dikin_phase1_lp(A,b);
[x,stats] = dikin_solver_lp(c,A,b,x0,false);

figure
set(gca, 'FontSize',18);
semilogy(stats)
h = legend('$\mbox{pos}~\left(-\min~(c+A^T\nu)\right)~$','$|c^T x+b^T\nu|$');
xlabel('iteration')
set(h, 'interpreter', 'latex')
print -depsc lp_short_step

% smaller example with long step
[x,stats] = dikin_solver_lp(c,A,b,x0,true);

figure
set(gca, 'FontSize',18);
semilogy(stats)
h = legend('$\mbox{pos}~\left(-\min~(c+A^T\nu)\right)~$','$|c^T x+b^T\nu|$');
xlabel('iteration')
set(h, 'interpreter', 'latex')
print -depsc lp_long_step

% larger example with long step
m = 1000;
n = 4000;
A = randn(m,n);
b = randn(m,1);
c = rand(n,1);

[x0,feas,stats] = dikin_phase1_lp(A,b);
[x,stats] = dikin_solver_lp(c,A,b,x0,true);

figure
set(gca, 'FontSize',18);
semilogy(stats)
h = legend('$\mbox{pos}~\left(-\min~(c+A^T\nu)\right)~$','$|c^T x+b^T\nu|$');
xlabel('iteration')
set(h, 'interpreter', 'latex')
print -depsc lp_big_long_step


% large example with sparse matrices
m = 4000;
n = 16000;
A = sprandn(m,n,.008);
b = randn(m,1);
c = rand(n,1);

[x0,feas,stats] = dikin_phase1_lp(A,b);
[x,stats] = dikin_solver_lp(c,A,b,x0,true);

figure
set(gca, 'FontSize',18);
semilogy(stats)
h = legend('$\mbox{pos}~\left(-\min~(c+A^T\nu)\right)~$','$|c^T x+b^T\nu|$');
xlabel('iteration')
set(h, 'interpreter', 'latex')
print -depsc lp_sparse_long_step


