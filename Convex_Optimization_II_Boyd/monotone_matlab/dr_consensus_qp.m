% Solves the QP
%       mininimze   (1/2)||Ax - b||_2^2
%       subject to  Fx <= g
% using D-R consensus. Note that the code has not been optimized for
% runtime and is only presened to give an idea of D-R consensu. For better
% performance, the inner loop should be run in parallel and should use a
% fast QP solver for small problems (e.g., CVXGEN).
%
% EE364b Convex Optimization II, S. Boyd
% Written by Eric Chu, 04/25/11
% 

close all; clear all
randn('state', 0); rand('state', 0);

%%% Generate problem instance
m = 1000;
n = 100;
k = 50;

xtrue = randn(n,1);
A = randn(m,n);
b = A*xtrue + randn(m,1);

F = randn(k,n);
g = F*xtrue;

%%% Use CVX to find solution
cvx_begin
    variable x(n)
    minimize ((1/2)*sum_square(A*x - b))
    subject to
        F*x <= g
cvx_end
xcvx = x;
fstar = cvx_optval; 
  
%%% Douglas-Rachford consensus splitting
N           = 10;      % number of subproblems
MAX_ITERS   = 50;
rho         = 200;

z           = zeros(n,N); 
xbar        = zeros(n,1);

for j = 1:MAX_ITERS,
    
    % x = prox_f(z), could be done in parallel
    for i = 1:N,
        Ai = A(m/N*(i-1) + 1:i*m/N,:);
        bi = b(m/N*(i-1) + 1:i*m/N);
        
        Fi = F(k/N*(i-1) + 1:i*k/N,:);
        gi = g(k/N*(i-1) + 1:i*k/N);
        
        % use CVX to solve prox operator
        zi = z(:,i);
        cvx_solver sdpt3
        cvx_begin quiet
            variable xi(n)
            minimize ( (1/2)*sum_square(Ai*xi - bi) + (rho/2)*sum_square(xi - zi) )
            subject to
                Fi*xi <= gi
        cvx_end
        x(:,i) = xi;
    end
    
    xbar_prev = xbar;
    xbar = mean(x,2);
    
    % record infeasibilities
    infeas(j) = sum(pos(F*xbar - g));
    
    % record objective value
    f(j) = (1/2)*sum_square(A*xbar - b);
    
    % update
    z = z + (xbar*ones(1,N) - x) + (xbar - xbar_prev)*ones(1,N);
end

%%% Make plots
subplot(2,1,1)
semilogy(1:MAX_ITERS, infeas);
ylabel('infeas'); set(gca, 'FontSize', 18); axis([1 MAX_ITERS 10^-2 10^2])
subplot(2,1,2)
plot(1:MAX_ITERS, f, [1 MAX_ITERS], [fstar fstar], 'k--');
xlabel('k'); ylabel('f'); axis([1 MAX_ITERS 300 2000]); set(gca, 'FontSize', 18);
print -depsc dr_consensus_qp.eps
