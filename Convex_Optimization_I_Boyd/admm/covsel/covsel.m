function [Z, history] = covsel(D, lambda, rho, alpha)

% covsel  Sparse inverse covariance selection via ADMM
%
% [X, history] = covsel(D, lambda, rho, alpha)
%
% Solves the following problem via ADMM:
%
%   minimize  trace(S*X) - log det X + lambda*||X||_1
%
% with variable X, where S is the empirical covariance of the data
% matrix D (training observations by features).
%
% The solution is returned in the matrix X.
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
% More information can be found in the paper linked at:
% http://www.stanford.edu/~boyd/papers/distr_opt_stat_learning_admm.html
%

t_start = tic;

QUIET    = 0;
MAX_ITER = 1000;
ABSTOL   = 1e-4;
RELTOL   = 1e-2;

S = cov(D);
n = size(S,1);

X = zeros(n);
Z = zeros(n);
U = zeros(n);

if ~QUIET
    fprintf('%3s\t%10s\t%10s\t%10s\t%10s\t%10s\n', 'iter', ...
      'r norm', 'eps pri', 's norm', 'eps dual', 'objective');
end

for k = 1:MAX_ITER

    % x-update
    [Q,L] = eig(rho*(Z - U) - S);
    es = diag(L);
    xi = (es + sqrt(es.^2 + 4*rho))./(2*rho);
    X = Q*diag(xi)*Q';

    % z-update with relaxation
    Zold = Z;
    X_hat = alpha*X + (1 - alpha)*Zold;
    Z = shrinkage(X_hat + U, lambda/rho);

    U = U + (X_hat - Z);

    % diagnostics, reporting, termination checks

    history.objval(k)  = objective(S, X, Z, lambda);

    history.r_norm(k)  = norm(X - Z, 'fro');
    history.s_norm(k)  = norm(-rho*(Z - Zold),'fro');

    history.eps_pri(k) = sqrt(n*n)*ABSTOL + RELTOL*max(norm(X,'fro'), norm(Z,'fro'));
    history.eps_dual(k)= sqrt(n*n)*ABSTOL + RELTOL*norm(rho*U,'fro');


    if ~QUIET
        fprintf('%3d\t%10.4f\t%10.4f\t%10.4f\t%10.4f\t%10.2f\n', k, ...
            history.r_norm(k), history.eps_pri(k), ...
            history.s_norm(k), history.eps_dual(k), history.objval(k));
    end

    if (history.r_norm(k) < history.eps_pri(k) && ...
       history.s_norm(k) < history.eps_dual(k))
         break;
    end
end

if ~QUIET
    toc(t_start);
end

end

function obj = objective(S, X, Z, lambda)
    obj = trace(S*X) - log(det(X)) + lambda*norm(Z(:), 1);
end

function y = shrinkage(a, kappa)
    y = max(0, a-kappa) - max(0, -a-kappa);
end

