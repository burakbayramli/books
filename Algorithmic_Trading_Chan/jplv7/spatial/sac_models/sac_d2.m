

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 10 seconds
% on an Anthalon 1200 Mhz
load elect.dat;             % load data on votes
y =  (elect(:,7)./elect(:,8));
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
% info.lflag = 1; % use MC approximation for lndet

result1 = sac(y,x,W,W);
prt(result1,vnames);

result2 = sac_gmm(y,x,W,W);
prt(result2,vnames);

prior.novi = 1;
ndraw = 2500;
nomit = 500;
result3 = sac_g(y,x,W,W,ndraw,nomit,prior);
prt(result3,vnames);
