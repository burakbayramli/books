% PURPOSE: An example of using mess_g3() on a large dataset
%          Bayesian matrix exponential spatial model
%---------------------------------------------------
% USAGE: mess_g3d2 (see mess_g3d for a small data set)
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
option.mmin = 25;
option.mmax = 37;
option.rmin = 0.8;
option.rmax = 1;
option.xflag = 1; % include spatial lags of x-variables
ndraw = 2000; % if you're impatient, decrease this
nomit = 1000; % and this as well, 1250 and 250 should work
res1 = mess_g3(y,x,option,ndraw,nomit);

prt(res1,vnames);

histo(res1.adraw);
title('posterior distribution for alpha');
pause;
histo(res1.rdraw);
title('posterior distribution for rho');
pause;
histo(res1.mdraw);
title('posterior distribution for neigh');
pause;

