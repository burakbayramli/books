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

vnames = strvcat('il','in','ky','mi','oh','pa','tn','wv', ...
    'd1','d2','d3','d4','d5','d6','d7','d8','d9','d10','d11');
     
y = test;
[nobs neqs] = size(y);

% generate seasonal dummy variables
dumm = sdummy(nobs,12);

xx = dumm(:,1:11);

nlag = 2;  % number of lags in var-model
tight = 0.1;
decay = 0.1;
weight = 0.5; % symmetric weights


% estimate the model
results = bvar(y,nlag,tight,weight,decay,xx);

% print results to a file
% fid = fopen('bvar.out','wr');
fid = 1;
prt(results,vnames,fid);




