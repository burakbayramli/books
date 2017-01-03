% PURPOSE: An example of using mess_g() on a large data set 
%          Bayesian matrix exponential spatial model                             
%---------------------------------------------------
% USAGE: mess_gd2 (see mess_gd for a small data set)
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

clear elect;
vnames = strvcat('voters','const','educ','homeowners','income');

% do max like for comparison
option.latt = latt;
option.long = long;
option.neigh = 5;
option.rho = 0.9;
%option.xflag = 1;

res0 = mess(y,x,option);
prt(res0,vnames);

ndraw = 5500;
nomit = 500;
res1 = mess_g(y,x,option,ndraw,nomit);

prt(res1,vnames);
