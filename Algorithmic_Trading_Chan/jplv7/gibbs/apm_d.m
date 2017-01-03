% PURPOSE: demo of apm()
%          Geweke's chi-squared test for MCMC convergence
% 
%---------------------------------------------------
% USAGE: apm_d
%---------------------------------------------------


n=100; k=3; % set number of observations and variables
randn('seed',10101);
x = randn(n,k); b = ones(k,1); % generate data set
randn('seed',20201);
y = x*b + randn(n,1);
ndraw = 300; nomit = 10; % set the number of draws   
r = [1.0 1.0 1.0]'; % prior b means
R = eye(k); 
T = eye(k);         % prior b variance 
rval = 2;           % hetroscedastic prior for r-value
prior.beta = r;
prior.bcov = T;
prior.rmat = R;
prior.rval = rval;
% get some Gibbs sampling output
result1 = ols_g(y,x,ndraw,nomit,prior);
% get some more Gibbs sampling output
result2 = ols_g(y,x,ndraw,nomit,prior);
% call momentg for both samples
resm1 = momentg(result1.bdraw);
resm2 = momentg(result2.bdraw);
% use apm to test the two samples for equality
% in the means
apm_res = apm(resm1,resm2);

vnames = strvcat('beta1','beta2','beta3');
prt_coda(apm_res,vnames);

