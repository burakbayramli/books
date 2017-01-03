% PURPOSE: An example of using mess_g()
%          Bayesian matrix exponential spatial model
%          on a large data set                    
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

[junk W junk] = xy2cont(latt,long);

% do Monte Carlo generation of an SAR model
sige = 10; 
randn('seed',0);
evec = randn(n,1)*sqrt(sige);
beta = ones(4,1);
rho = 0.6; A = eye(n) - rho*W;  AI = inv(A);
y = AI*x*beta + AI*evec; % generate some data

% do max like for comparison
option1.latt = latt;
option1.long = long;
option1.neigh = 5;
option1.rho = 0.9;
%option1.xflag = 1;

res0 = mess(y,x,option1);
prt(res0);

option.latt = latt;
option.long = long;
option.neigh = 5;
option.rmin = 0.6;
option.rmax = 1;
ndraw = 1100;
nomit = 100;
res1 = mess_g2(y,x,option,ndraw,nomit);

res1
