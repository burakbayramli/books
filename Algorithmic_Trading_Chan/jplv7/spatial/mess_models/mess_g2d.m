% PURPOSE: An example of using mess_g2()
%          Bayesian matrix exponential spatial model
%          on a small data set                    
%---------------------------------------------------
% USAGE: mess_g2d (see mess_g2d2 for a large data set)
%---------------------------------------------------

clear all;

% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
n = length(anselin);
x = [ones(n,1) anselin(:,2:3)]; 
latt = anselin(:,4);
long = anselin(:,5);
vnames = strvcat('crime','constant','income','hvalue');

[junk W junk] = xy2cont(latt,long);

% do Monte Carlo generation of an SAR model
sige = 100; 
randn('seed',0);
evec = randn(n,1)*sqrt(sige);
beta = ones(3,1);
rho = 0.6; A = eye(n) - rho*W;  AI = inv(A);
y = AI*x*beta + AI*evec; % generate some data


option.latt = latt;
option.long = long;
option.neigh = 5;
option.rmin = 0.5;
option.rmax = 1;
ndraw = 2200;
nomit = 200;
res1 = mess_g2(y,x,option,ndraw,nomit);
prt(res1,vnames);

option.xflag = 1;
res2 = mess_g2(y,x,option,ndraw,nomit);
prt(res2,vnames);

