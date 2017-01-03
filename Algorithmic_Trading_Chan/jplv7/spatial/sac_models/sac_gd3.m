% PURPOSE: An example of using sac() on a large data set  
%          general spatial model                           
%---------------------------------------------------
% USAGE: sac_d2 (see sac_d for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 10 seconds
% on an Anthalon 1200 Mhz
load elect.dat;             % load data on votes
latt = elect(:,5);
long = elect(:,6);
n = 3107;
[j1 W j2] = xy2cont(latt,long);

k = 3;
beta = [1
        -1
        1];
    
rho = 0.45;
lam = -.4;

x = randn(n,k);


A = speye(n) - rho*sparse(W);
B = speye(n) - lam*sparse(W);


evec = randn(n,1);

y = A\(x*beta) + (B*A)\evec;


vnames = strvcat('y','x1=1','x2= -1','x3 = 1');

% info.pflag = 1; % a flag to print intermediate optimization results
info.lflag = 1; % use MC approximation for lndet

info.ndraw = 2500;
result1 = sac(y,x,W,W,info);
prt(result1,vnames);

options.ndraw = 2500;
result2 = sac_gmm(y,x,W,W,options);
prt(result2,vnames);

prior.novi = 1;
ndraw = 5500;
nomit = 2000;

result3 = sac_g(y,x,W,W,ndraw,nomit,prior);
result3.tflag = 'tstat';
prt(result3,vnames);


