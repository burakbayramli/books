rand('seed', 0);
randn('seed', 0);

n = 100;

x0 = ones(n,1);
for j = 1:3
    idx = randsample(n,1);
    k = randsample(1:10,1);
    x0(ceil(idx/2):idx) = k*x0(ceil(idx/2):idx);
end
b = x0 + randn(n,1);

disp(x0);

lambda = 5;

e = ones(n,1);
D = spdiags([e -e], 0:1, n,n);

[x history] = total_variation(b, lambda, 1.0, 1.0);

disp(x);
