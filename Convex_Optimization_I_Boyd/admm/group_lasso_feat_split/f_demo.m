randn('seed', 0);
rand('seed',0);

m = 200;        % amount of data
K = 200;        % number of blocks
ni = 100;       % size of each block

n = ni*K;
p = 10/K;      % sparsity density

% generate block sparse solution vector
x = zeros(ni,K);
for i = 1:K,
    if( rand() < p)
        % fill nonzeros
        x(:,i) = randn(ni,1);
    end
end
x = vec(x);

% generate random data matrix
A = randn(m,n);

% normalize columns of A
A = A*spdiags(1./norms(A)',0,n,n);

% generate measurement b with noise
b = A*x + sqrt(1)*randn(m,1);

% lambda max
for i = 1:K,
    Ai = A(:,(i-1)*ni + 1:i*ni);
    nrmAitb(i) = norm(Ai'*b);
end
lambda_max = max( nrmAitb );

% regularization parameter
lambda = 0.5*lambda_max;

xtrue = x;   % save solution

[x history] = group_lasso_feat_split(A, b, lambda, ni, 10, 1.0);

