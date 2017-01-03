clear all ; close all ; clc
randn('seed',0);
rand('seed',0);

gramian = @(A) (A'*A);
randpsd = @(n) gramian(randn(n));

m = 3;
n = 5;
k = 12;
T = 10;

p = rand(T,k);
for t = 1:T
    p(t,:) = p(t,:) / sum(p(t,:));
end

A = cell(T,k);
B = cell(T,k);
c = cell(T,k);
P = cell(T,k);
q = cell(T,k);
r = cell(T,k);
for t = 1:T
    for w = 1:k
        A{t,w} = randn(n,n);
        B{t,w} = randn(n,m);
        c{t,w} = randn(n,1);
        P{t,w} = randpsd(m+n);
        q{t,w} = randn(m+n,1);
        r{t,w} = randn();
    end
end

pT = randpsd(n);
qT = randn(n,1);
rT = randn();

x0 = randn(n,1);
