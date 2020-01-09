% Dikin's method: SDP examples
% produces figures for Dikin method slides
% 
% EE364b Convex Optimization II, Stephen Boyd
% written by AJ Friend 4/8/2014
%

rng(0);
n = 100;
m = 100;

A = randn(m,m,n);
for i=1:n
   A(:,:,i) =  A(:,:,i) + A(:,:,i)';
end
B = randn(m,m);
B = B*B';
c = randn(n,1);

% CVX reference solution
cvx_begin quiet
    variable xstar(n)
    minimize( c'*xstar )
    S = zeros(m,m);
    for i=1:n
        S = S + A(:,:,i)*xstar(i);
    end
    B - S == semidefinite(m);
cvx_end

% normalize problem so |c'*xstar| = 1
c = c/abs(c'*xstar);

% CVX gives us a feasible starting point
cvx_begin quiet
    variable x(n)
    S = zeros(m,m);
    for i=1:n
        S = S + A(:,:,i)*x(i);
    end
    B - S == semidefinite(m);
cvx_end

[x,stats] = dikin_solver_sdp(c,A,B,x);

figure
set(gca, 'FontSize',18);
semilogy(stats(:,1:2))
h = legend('$\mbox{pos}~(-\lambda_{min}~(Z))~$','$|c^T x + \mbox{tr}~(BZ)|~$');
set(h, 'interpreter', 'latex')
print -depsc sdp_example

