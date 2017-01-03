% PURPOSE: An example of using bvar_g(), 
%          Gibbs estimates for a vector autoregressive model                                                 
%          (with Minnesota prior)                    
%---------------------------------------------------
% USAGE: bvar_gd
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5

vnames =  ['  il',
           '  in'];  

% vnames = strvcat('il','in','ky','mi','oh','pa','tn','wv');
     
y = test(:,1:2); % use only two variables
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
prior.rval = 50;
% rely on default rval = 4
ndraw = 1250;
nomit = 250;

res1 = bvar(y,nlag,tight,weight,decay);
% fid = fopen('bvarg.out','w');
fid = 1;
prt(res1,vnames,fid);

% estimate the model
results = bvar_g(y,nlag,ndraw,nomit,prior);
prt_varg(results,vnames,fid);
plt_varg(results,vnames);

plot(results(1).bdraw);
title('beta draws for equation 1');
pause;
plot(results(2).bdraw);
title('beta draws for equation 2');


