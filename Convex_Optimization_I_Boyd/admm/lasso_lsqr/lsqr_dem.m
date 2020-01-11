randn('seed', 0);
rand('seed',0);

m  = 1000000;   % number of examples
n  = 10000;     % number of features
p1 = 0.001;     % sparsity density of solution vector
p2 = 0.0001;    % sparsity density of A

x0 = sprandn(n, 1, p1);
A = sprandn(m, n, p2);
b = A*x0 + 0.1*randn(m,1);

lambda_max = norm(A'*b, 'inf');
lambda = 0.1*lambda_max;

[x history] = lasso_lsqr(A, b, lambda, 1.0, 1.0);

