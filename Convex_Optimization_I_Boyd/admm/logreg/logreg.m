function [z, history] = logreg(A, b, mu, rho, alpha)

% logreg   Solve L1 regularized logistic regression via ADMM
%
% [z, history] = logreg(A, b, mu, rho, alpha)
%
% solves the following problem via ADMM:
%
%   minimize   sum( log(1 + exp(-b_i*(a_i'w + v)) ) + m*mu*norm(w,1)
%
% where A is a feature matrix and b is a response vector. The scalar m is
% the number of examples in the matrix A.
%
% This solves the L1 regularized logistic regression problem. It uses
% a custom Newton solver for the x-step.
%
% The solution is returned in the vector x = (v,w).
%
% history is a structure that contains the objective value, the primal and
% dual residual norms, and the tolerances for the primal and dual residual
% norms at each iteration.
%
% rho is the augmented Lagrangian parameter.
%
% alpha is the over-relaxation parameter (typical values for alpha are
% between 1.0 and 1.8).
%
%
% More information can be found in the paper linked at:
% http://www.stanford.edu/~boyd/papers/distr_opt_stat_learning_admm.html
%

t_start = tic;

QUIET    = 0;
MAX_ITER = 1000;
ABSTOL   = 1e-4;
RELTOL   = 1e-2;

[m, n] = size(A);

x = zeros(n+1,1);
z = zeros(n+1,1);
u = zeros(n+1,1);


if ~QUIET
    fprintf('%3s\t%10s\t%10s\t%10s\t%10s\t%10s\n', 'iter', ...
      'r norm', 'eps pri', 's norm', 'eps dual', 'objective');
end

for k = 1:MAX_ITER

    % x-update
    x = update_x(A, b, u, z, rho);

    % z-update with relaxation
    zold = z;
    x_hat = alpha*x + (1-alpha)*zold;
    z = x_hat + u;
    z(2:end) = shrinkage(z(2:end), (m*mu)/rho);

    u = u + (x_hat - z);

    % diagnostics, reporting, termination checks
    history.objval(k)  = objective(A, b, mu, x, z);

    history.r_norm(k)  = norm(x - z);
    history.s_norm(k)  = norm(rho*(z - zold));


    history.eps_pri(k) = sqrt(n)*ABSTOL + RELTOL*max(norm(x), norm(z));
    history.eps_dual(k)= sqrt(n)*ABSTOL + RELTOL*norm(rho*u);

    if ~QUIET
        fprintf('%3d\t%10.4f\t%10.4f\t%10.4f\t%10.4f\t%10.2f\n', k, ...
            history.r_norm(k), history.eps_pri(k), ...
            history.s_norm(k), history.eps_dual(k), history.objval(k));
    end


    if history.r_norm(k) < history.eps_pri(k) && ...
       history.s_norm(k) < history.eps_dual(k)
        break;
    end
end

if ~QUIET
    toc(t_start);
end

end

function obj = objective(A, b, mu, x, z)
    m = size(A,1);
    obj = sum(log(1 + exp(-A*x(2:end) - b*x(1)))) + m*mu*norm(z,1);
end

function x = update_x(A, b, u, z, rho, x0)
    % solve the x update
    %   minimize [ -logistic(x_i) + (rho/2)||x_i - z^k + u^k||^2 ]
    % via Newton's method; for a single subsystem only.
    alpha = 0.1;
    BETA  = 0.5;
    TOLERANCE = 1e-5;
    MAX_ITER = 50;
    [m n] = size(A);
    I = eye(n+1);
    if exist('x0', 'var')
        x = x0;
    else
        x = zeros(n+1,1);
    end
    C = [-b -A];
    f = @(w) (sum(log(1 + exp(C*w))) + (rho/2)*norm(w - z + u).^2);
    for iter = 1:MAX_ITER
        fx = f(x);
        g = C'*(exp(C*x)./(1 + exp(C*x))) + rho*(x - z + u);
        H = C' * diag(exp(C*x)./(1 + exp(C*x)).^2) * C + rho*I;
        dx = -H\g;   % Newton step
        dfx = g'*dx; % Newton decrement
        if abs(dfx) < TOLERANCE
            break;
        end
        % backtracking
        t = 1;
        while f(x + t*dx) > fx + alpha*t*dfx
            t = BETA*t;
        end
        x = x + t*dx;
    end
end
function z = shrinkage(a, kappa)
    z = max(0, a-kappa) - max(0, -a-kappa);
end


