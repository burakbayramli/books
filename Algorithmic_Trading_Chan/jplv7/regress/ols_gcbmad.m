% PURPOSE: Demo of ols_gcbma() model comparison function
% Fins posterior model probabilities for different OLS models based on varying X matrices
% known as Markov Chain Monte Carlo Model Comparison (MC^3)

clear all;

n = 100;
kin = 3;
kout = 10;

xo = randn(n,kin);
x = [ones(n,1) xo];
z = randn(n,kout);
[n,k] = size(x);
vnames = strvcat('y','constant','x1','x2','x3', ...
    'x1out','x2out','x3out','x4out','x5out','x6out','x7out','x8out','x9out','x10out');
sige = 1;
b = ones(kin+1,1);
b(1,1) = 1.0;
y = (x*b) + randn(n,1)*sqrt(sige);

res = ols(y,[x z]);
prt(res,vnames);

ndraw = 10000;
prior.nmodels = 10;
prior.g = 1/(n*n);

results = ols_gcbma(y,[xo z],ndraw,prior);
results

prt_bmao(results,vnames);



