rand('seed', 0);
randn('seed', 0);

m = 1000; % number of examples
n = 100;  % number of features

A = randn(m,n);
x0 = 10*randn(n,1);
b = A*x0;
idx = randsample(m,ceil(m/50));
b(idx) = b(idx) + 1e2*randn(size(idx));

[x history] = lad(A, b, 1.0, 1.0);

