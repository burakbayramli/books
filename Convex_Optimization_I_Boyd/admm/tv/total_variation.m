function [x, history] = total_variation(b, lambda, rho, alpha)

% total_variation  Solve total variation minimization via ADMM
%
% [x, history] = total_variation(b, lambda, rho, alpha)
%
% Solves the following problem via ADMM:
%
%   minimize  (1/2)||x - b||_2^2 + lambda * sum_i |x_{i+1} - x_i|
%
% where b in R^n.
%
% The solution is returned in the vector x.
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
% https://web.stanford.edu/~boyd/papers/admm_distr_stats.html
%

t_start = tic;

QUIET    = 0;
MAX_ITER = 1000;
ABSTOL   = 1e-4;
RELTOL   = 1e-2;

n = length(b);

% difference matrix
e = ones(n,1);
D = spdiags([e -e], 0:1, n,n);

x = zeros(n,1);
z = zeros(n,1);
u = zeros(n,1);

fprintf('%3s\t%10s\t%10s\t%10s\t%10s\t%10s\n', 'iter', ...
      'r norm', 'eps pri', 's norm', 'eps dual', 'objective');

I = speye(n);
DtD = D'*D;

for k = 1:MAX_ITER

    % x-update
    x = (I + rho*DtD) \ (b + rho*D'*(z-u));

    % z-update with relaxation
    zold = z;
    Ax_hat = alpha*D*x +(1-alpha)*zold;
    z = shrinkage(Ax_hat + u, lambda/rho);

    % y-update
    u = u + Ax_hat - z;


    % diagnostics, reporting, termination checks
    history.objval(k)  = objective(b, lambda, D, x, z);

    history.r_norm(k)  = norm(D*x - z);
    history.s_norm(k)  = norm(-rho*D'*(z - zold));

    history.eps_pri(k) = sqrt(n)*ABSTOL + RELTOL*max(norm(D*x), norm(-z));
    history.eps_dual(k)= sqrt(n)*ABSTOL + RELTOL*norm(rho*D'*u);

    fprintf('%3d\t%10.4f\t%10.4f\t%10.4f\t%10.4f\t%10.2f\n', k, ...
        history.r_norm(k), history.eps_pri(k), ...
        history.s_norm(k), history.eps_dual(k), history.objval(k));

    if (history.r_norm(k) < history.eps_pri(k) && ...
       history.s_norm(k) < history.eps_dual(k))
         break;
    end
end

toc(t_start);

end

