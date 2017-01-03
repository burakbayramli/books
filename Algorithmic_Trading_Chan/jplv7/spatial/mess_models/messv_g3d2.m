% PURPOSE: An example of using messv_g3() on a large data set
%          Bayesian matrix exponential spatial model                              
%---------------------------------------------------
% USAGE: messv_g3d2 (see messv_g3d for a small data set)
%---------------------------------------------------
clear all;
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
option.rmin = .8;
option.rmax = 1;
option.mmin = 2;
option.mmax = 10;
option.xflag = 0;
%option.xflag = 1; % include spatial lags of x-variables
ndraw = 1100;
nomit = 100;

res1 = messv_g3(y,x,option,ndraw,nomit);

prt(res1,vnames);
