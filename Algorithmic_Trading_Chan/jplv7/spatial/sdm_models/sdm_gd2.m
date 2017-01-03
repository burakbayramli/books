% PURPOSE: An example of using sdm_g() on a large data set
%          Bayesian heteroscedastic spatial Durbin model                              
%---------------------------------------------------
% USAGE: sdm_gd2 (see sdm_gd for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations from Pace and Barry
load elect.dat;             % load data on votes
y =  log(elect(:,7)./elect(:,8));
x1 = log(elect(:,9)./elect(:,8));
x2 = log(elect(:,10)./elect(:,8));
x3 = log(elect(:,11)./elect(:,8));
n = length(y); x = [ones(n,1) x1 x2 x3];
xc = elect(:,5);
yc = elect(:,6);
clear x1; clear x2; clear x3;
clear elect;                % conserve on RAM memory
[j1 W j2] = xy2cont(xc,yc);

n = 3107;
vnames = strvcat('voters','const','educ','homeowners','income');

% Gibbs sampling function homoscedastic prior
prior.novi = 1; % homoscedastic prior 
ndraw = 2500;
nomit = 500;

results = sdm_g(y,x,W,ndraw,nomit,prior);
results.tflag = 'tstat';
prt(results,vnames);

% rely on default heteroscedastic prior
results2 = sdm_g(y,x,W,ndraw,nomit);
prt(results2,vnames);