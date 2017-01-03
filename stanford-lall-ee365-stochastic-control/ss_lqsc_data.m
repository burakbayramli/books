randn('seed',0);
m = 3;
n = 2;

gramian = @(A) A'*A;
randpsd = @(n) gramian(randn(n));

A = randn(n,n);
B = randn(n,m);
P = randpsd(n+m);
q = randn(n+m,1);
r = randn();

wbar = randn(n,1);
wvar = randpsd(n);