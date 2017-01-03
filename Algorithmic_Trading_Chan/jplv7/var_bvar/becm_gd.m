% PURPOSE: An example of using becm_g(), 
%          Gibbs estimates for a error correction model                                                 
%          (with Minnesota prior)                    
%---------------------------------------------------
% USAGE: becm_gd
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5

vnames =  strvcat('il','in','ky','mi','oh','pa','tn','wv');

y = test;
[nobs neqs] = size(y);

nlag = 2;  % number of lags in var-model
tight = 0.1;
decay = 0.1;
weight = 0.5; % symmetric weights

% this is an example of using 1st-order contiguity
% of the states as weights as in LeSage and Pan (1995)
% `Using Spatial Contiguity as Bayesian Prior Information 
% in Regional Forecasting Models'' International Regional 
% Science Review, Volume 18, no. 1, pp. 33-53, 1995.

w = [1.0  1.0  1.0  0.1  0.1  0.1  0.1  0.1 
     1.0  1.0  1.0  1.0  1.0  0.1  0.1  0.1 
     1.0  1.0  1.0  0.1  1.0  0.1  1.0  1.0 
     0.1  1.0  0.1  1.0  1.0  0.1  0.1  0.1 
     0.1  1.0  1.0  1.0  1.0  1.0  0.1  1.0 
     0.1  0.1  0.1  0.1  1.0  1.0  0.1  1.0 
     0.1  0.1  1.0  0.1  0.1  0.1  1.0  0.1 
     0.1  0.1  1.0  0.1  1.0  1.0  0.1  1.0];

% set up prior structure
prior.tight = tight;
prior.decay = decay;
prior.weight = weight;
%prior.rval = 20;
% rely on default rval = 4
ndraw = 400;
nomit = 50;

res1 = becm(y,nlag,tight,weight,decay);
fid = fopen('becmg.out','w');
prt(res1,vnames,fid);

% estimate the model using Johansen determined co-integrating relations
results = becm_g(y,nlag,prior,ndraw,nomit);
prt_varg(results,vnames,fid);
fclose(fid);
plt_varg(results,vnames);

