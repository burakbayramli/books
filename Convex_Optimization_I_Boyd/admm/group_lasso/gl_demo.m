randn('seed', 0);
rand('seed',0);

m = 1500;       % amount of data
K = 200;        % number of blocks
partition = randi(50, [K 1]);

n = sum(partition); % number of features
p = 100/n;          % sparsity density

% generate block sparse solution vector
x = zeros(n,1);
start_ind = 1;
cum_part = cumsum(partition);
for i = 1:K,
    x(start_ind:cum_part(i)) = 0;
    if( rand() < p)
        % fill nonzeros
        x(start_ind:cum_part(i)) = randn(partition(i),1);
    end
    start_ind = cum_part(i)+1;
end

% generate random data matrix
A = randn(m,n);

% normalize columns of A
A = A*spdiags(1./norms(A)',0,n,n);

% generate measurement b with noise
b = A*x + sqrt(0.001)*randn(m,1);

% lambda max
start_ind = 1;
for i = 1:K,
    sel = start_ind:cum_part(i);
    lambdas(i) = norm(A(:,sel)'*b);
    start_ind = cum_part(i) + 1;
end
lambda_max = max(lambdas);

% regularization parameter
lambda = 0.1*lambda_max;

xtrue = x;   % save solution

[x history] = group_lasso(A, b, lambda, partition, 1.0, 1.0);
