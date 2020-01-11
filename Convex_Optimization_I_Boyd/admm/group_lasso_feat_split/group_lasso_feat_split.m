function [x, history] = group_lasso_feat_split(A, b, lambda, ni, RHO, ALPHA)

% group_lasso_feat_split  Solve group lasso problem via ADMM feature splitting
%
% [x, history] = group_lasso_feat_split(A, b, p, lambda, rho, alpha);
%
% solves the following problem via ADMM:
%
%   minimize 1/2*|| Ax - b ||_2^2 + \lambda sum(norm(x_i))
%
% The input p is a K-element vector giving the block sizes n_i, so that x_i
% is in R^{n_i}.
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
% This version is a (serially) distributed, feature splitting example.
%
%
% More information can be found in the paper linked at:
% http://www.stanford.edu/~boyd/papers/distr_opt_stat_learning_admm.html
%

t_start = tic;

QUIET    = 0;
MAX_ITER = 100;
RELTOL  = 1e-2;
ABSTOL   = 1e-4;

[m, n] = size(A);

% check that ni divides in to n
if (rem(n,ni) ~= 0)
    error('invalid block size');
end
% number of subsystems
N = n/ni;

rho = RHO;
alpha = ALPHA;    % over-relaxation parameter

x = zeros(ni,N);
z = zeros(m,1);
u = zeros(m,1);
Axbar = zeros(m,1);

zs = zeros(m,N);
Aixi = zeros(m,N);

if ~QUIET
    fprintf('%3s\t%10s\t%10s\t%10s\t%10s\t%10s\n', 'iter', ...
      'r norm', 'eps pri', 's norm', 'eps dual', 'objective');
end

% pre-factor
for i = 1:N,
    Ai = A(:,(i-1)*ni + 1:i*ni);
    [Vi,Di] = eig(Ai'*Ai);
    V{i} = Vi;
    D{i} = diag(Di);

    % in Matlab, transposing costs space and flops
    % so we save a transpose operation everytime
    At{i} = Ai';
end

for k = 1:MAX_ITER
    % x-update (to be done in parallel)
    for i = 1:N,
        Ai = A(:,(i-1)*ni + 1:i*ni);
        xx = x_update(Ai, Aixi(:,i) + z - Axbar - u, lambda/rho, V{i}, D{i});
        x(:,i) = xx;
        Aixi(:,i) = Ai*x(:,i);
    end

    % z-update
    zold = z;
    Axbar = 1/N*A*vec(x);

    Axbar_hat = alpha*Axbar + (1-alpha)*zold;
    z = (b + rho*(Axbar_hat + u))/(N+rho);

    % u-update
    u = u + Axbar_hat - z;

    % compute the dual residual norm square
    s = 0; q = 0;
    zsold = zs;
    zs = z*ones(1,N) + Aixi - Axbar*ones(1,N);
    for i = 1:N,
        % dual residual norm square
        s = s + norm(-rho*At{i}*(zs(:,i) - zsold(:,i)))^2;
        % dual residual epsilon
        q = q + norm(rho*At{i}*u)^2;
    end

    % diagnostics, reporting, termination checks
    history.objval(k)  = objective(A, b, lambda, N, x, z);
    history.r_norm(k)  = sqrt(N)*norm(z - Axbar);
    history.s_norm(k)  = sqrt(s);

    history.eps_pri(k) = sqrt(n)*ABSTOL + RELTOL*max(norm(Aixi,'fro'), norm(-zs, 'fro'));
    history.eps_dual(k)= sqrt(n)*ABSTOL + RELTOL*sqrt(q);


    if ~QUIET
        fprintf('%3d\t%10.4f\t%10.4f\t%10.4f\t%10.4f\t%10.2f\n', k, ...
            history.r_norm(k), history.eps_pri(k), ...
            history.s_norm(k), history.eps_dual(k), history.objval(k));
    end

    if history.r_norm(k) < history.eps_pri(k) && ...
       history.s_norm(k) < history.eps_dual(k);
        break
    end

end

if ~QUIET
    toc(t_start);
end

end















