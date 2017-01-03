% PURPOSE: compute posterior probabilities of
% different SAR models based on varying X matrices

clear all;

load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));

% latt = anselin(:,4);
% long = anselin(:,5);
% [junk W junk] = xy2cont(latt,long);
[n junk] = size(W);


load anselin.dat; % standardized 1st-order spatial weight matrix
y = anselin(:,1);
n = length(y);
kin = 3;
kout = 5;

xo = randn(n,kin);
x = [ones(n,1) xo];
z = randn(n,kout);
[n,k] = size(x);
vnames = strvcat('y','constant','x1','x2','x3','x1out','x2out','x3out','x4out','x5out');
rho = 0.6;
sige = 0.1;
b = ones(kin+1,1);
b(1,1) = 5.0;
y = (x*b) + (speye(n) - rho*W)\(randn(n,1)*sqrt(sige));

res = ols(y,[x z]);
prt(res,vnames);

res2 = sem(y,[x z],W);
prt(res2);

ndraw = 10000;
prior.rmin = -1.0;
prior.rmax = 1.0;
prior.nmodels = 10;

res3 = sem_gcbma(y,[xo z],W,ndraw,prior);
prt_bmae(res3);

ind = find(res3.mprob > 0.001);
allmodels = res3.allmodels(ind,:);

res3.vprob = sum(allmodels)/length(allmodels);
res3.freq = sum(allmodels);
prt_bmae(res3);


res4 = ols_gcbma(y,[xo z],ndraw,prior);
prt_bmao(res4);

ind = find(res4.mprob > 0.001);
allmodels = res4.allmodels(ind,:);

res4.vprob = sum(allmodels)/length(allmodels);
res4.freq = sum(allmodels);
prt_bmao(res4);

