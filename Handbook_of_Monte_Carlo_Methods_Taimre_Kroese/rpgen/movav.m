%movav.m
pars = [1:20]';
b = [pars;1];
q = numel(pars);
N = 10^3;
eps = randn(q+1,1);
for i=1:N
    X(i) = b'*eps;
    eps = [eps(2:q+1);randn];
end
plot(X)
