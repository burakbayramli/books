% PURPOSE: An example of using bvar(), 
%          to estimate a vector autoregressive model                                                 
%          (with Minnesota prior)                    
%---------------------------------------------------
% USAGE: bvar_d
%---------------------------------------------------

load test.dat; % a test data set containing
               % monthly mining employment for
               % il,in,ky,mi,oh,pa,tn,wv
% data covers 1982,1 to 1996,5

vnames =  ['  il',
           '  in',    
           '  ky',    
           '  mi',    
           '  oh',    
           '  pa',    
           '  tn',    
           '  wv'];    
     
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

% estimate the model
results = bvar(y,nlag,tight,w,decay);

% print results to a file
% fid = fopen('bvar.out','wr');
fid = 1;
prt_var(results,vnames,fid);




