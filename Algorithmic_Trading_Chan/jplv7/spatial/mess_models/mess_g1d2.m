% PURPOSE: An example of using mess_g1() on a large data set 
%          Bayesian matrix exponential spatial model                            
%---------------------------------------------------
% USAGE: mess_g1d2 (see mess_g1d for a small data set)
%---------------------------------------------------

% NOTE a large data set with 3107 observations
% from Pace and Barry, 
% test MCMC sampling on pace and barry data set

clear all;
load elect.dat;             % load data on votes
y =  (elect(:,7)./elect(:,8));
x1 = log(elect(:,9)./elect(:,8));
x2 = log(elect(:,10)./elect(:,8));
x3 = log(elect(:,11)./elect(:,8));
n = length(y); x = [ones(n,1) x1 x2 x3];
latt = elect(:,5);
long = elect(:,6);
vnames = strvcat('voters','const','educ','homeowners','income');

clear elect;

option.latt = latt;
option.long = long;
option.rho = 1;
option.mmin = 15;
option.mmax = 25;
prior.alpha = -1;
prior.acov = 0.01;
option.xflag = 1;

ndraw = 1100;
nomit = 100;
res1 = mess_g1(y,x,option,ndraw,nomit);
prt(res1,vnames);

