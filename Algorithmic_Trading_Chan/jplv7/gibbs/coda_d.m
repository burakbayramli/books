% PURPOSE: demo of coda()
%          MCMC convergence diagnostics calculations
% 
%---------------------------------------------------
% USAGE: coda_d
%---------------------------------------------------

n=100; k=3; % set number of observations and variables
randn('seed',10101);
x = randn(n,k); b = ones(k,1); % generate data set
randn('seed',20201);
y = x*b + randn(n,1);
ndraw = 600; nomit = 100; % set the number of draws   
r = [1.0 1.0 1.0]'; % prior b means
R = eye(k); 
T = eye(k);         % prior b variance 
rval = 2;           % hetroscedastic prior for r-value
prior.beta = r;
prior.bcov = T;
prior.rmat = R;
prior.rval = rval;
% get some MCMC draws
result = ols_g(y,x,ndraw,nomit,prior);
vnames = strvcat('beta1','beta2','beta3');
% we can print results using prt also
res = coda(result.bdraw);
prt_coda(res,vnames);
% or print results using nargout = 0
coda(result.bdraw,vnames);

% we can change default options
in.q = 0.025; in.r = 0.01; in.s = 0.95;
coda(result.bdraw,vnames,in);
