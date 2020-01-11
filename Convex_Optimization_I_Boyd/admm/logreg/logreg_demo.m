rand('seed', 0);
randn('seed', 0);

n = 50;
m = 200;

w = sprandn(n, 1, 0.1);  % N(0,1), 10% sparse
v = randn(1);            % random intercept

X = sprandn(m, n, 10/n);
btrue = sign(X*w + v);

% noise is function of problem size use 0.1 for large problem
b = sign(X*w + v + sqrt(0.1)*randn(m,1)); % labels with noise

A = spdiags(b, 0, m, m) * X;

ratio = sum(b == 1)/(m);
mu = 0.1 * 1/m * norm((1-ratio)*sum(A(b==1,:),1) + ratio*sum(A(b==-1,:),1), 'inf');

x_true = [v; w];

[x history] = logreg(A, b, mu, 1.0, 1.0);

