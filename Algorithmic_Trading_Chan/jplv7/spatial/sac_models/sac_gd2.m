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
y =  log(elect(:,7)./elect(:,8));
x1 = log(elect(:,9)./elect(:,8));
x2 = log(elect(:,10)./elect(:,8));
x3 = log(elect(:,11)./elect(:,8));
n = length(y); x = [ones(n,1) x1 x2 x3];
clear x1; clear x2; clear x3;
latt = elect(:,5);
long = elect(:,6);
clear elect;                % conserve on RAM memory
n = 3107;
[j1 W j2] = xy2cont(latt,long);

vnames = strvcat('voters','const','educ','homeowners','income');

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
prt(result3,vnames);


