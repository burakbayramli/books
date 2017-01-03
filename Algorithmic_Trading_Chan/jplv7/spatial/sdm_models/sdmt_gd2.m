% PURPOSE: An example of using sdmt_g() on a large data set   
%          Gibbs sampling spatial durbin tobit model                         
%---------------------------------------------------
% USAGE: sdmt_gd2 (see sdmt_gd for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, 
load elect.dat;             % load data on votes
latt = elect(:,5);
long = elect(:,6);
n = length(latt);
k = 4;
x = randn(n,k);
clear elect;                % conserve on RAM memory
n = 3107;
[junk W junk] = xy2cont(latt,long);
vnames = strvcat('voters','const','educ','homeowners','income');

rho = 0.7;
beta = ones(k,1);
beta(1,1) = -1.0;
beta(2,1) = 1.0;
beta(3,1) = -1.0;
beta(4,1) = 1.0;
gamma = 0.5*beta;
sige = 1;

y = (speye(n) - rho*W)\(x*beta + W*x*gamma) + (speye(n) - rho*W)\randn(n,1)*sqrt(sige);
limit = mean(y);
ysave = y;
ind = find(y < limit);
y(ind,1) = limit; % censored  values

% do Gibbs sampling estimation
ndraw = 1250; 
nomit = 250;
prior.novi = 1;

res = sdm_g(ysave,x,W,ndraw,nomit,prior); % MCMC estimates based on
prt(res);                                  % non-truncated data for comparison

prior.limit = limit;
prior.trunc = 'left';
resg = sdmt_g(y,x,W,ndraw,nomit,prior); % tobit estimates
prt(resg,vnames);

tt=1:n;
plot(tt,ysave,tt,resg.ymean,'.');
pause;

plot(resg.pdraw);
title('rho draws');
pause;
plot(resg.bdraw);
title('beta draws');