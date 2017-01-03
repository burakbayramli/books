randn('seed',0);
rand('seed',0);
A = randn(10,5);
b = randn(10,1);
x = randn(5,1);

gramian = @(A) A'*A;
randpsd = @(n) gramian(randn(n));
P = randpsd(8);
q = randn(8,1);
r = randn();
y = randn(3,1);

nyvals = 10;
yvals = cell(nyvals,1);
for i = 1:nyvals
    yvals{i} = y + sqrtm(randpsd(3)) * randn(3,1);
end
ypmf = rand(nyvals,1);
ypmf = ypmf / sum(ypmf);
