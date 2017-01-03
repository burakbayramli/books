% PURPOSE: An example of using sdmt_g() Gibbs sampling
%          spatial durbin tobit model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: sdmt_gd (see also sdmt_gd2 for a large data set)
%---------------------------------------------------

clear all;

% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; % standardized 1st-order spatial weight matrix
latt = anselin(:,4);
long = anselin(:,5);
[junk W junk] = xy2cont(latt,long);
[n junk] = size(W);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 1;
k = 2;
x = randn(n,k);
beta(1,1) = -1.0;
beta(2,1) = 1.0;
gamma = 0.5*beta;

% generate SAR model
y = (IN-rho*W)\(x*beta + W*x*gamma) + (IN-rho*W)\(randn(n,1)*sqrt(sige)); 
ysave = y;

res = sdm(ysave,x,W); % maximum likelihood estimates
prt(res);             % based on non-truncated data

limit = 0;
ind = find(y < limit);
if length(ind) > 0
y(ind,1) = limit; % censor  values
else
    error('no censored values');
end;


prior.novi = 1;  % homoscedastic prior
prior.limit = limit;
prior.trunc = 'left';
ndraw = 2500;
nomit = 500;

res0 = sdm_g(ysave,x,W,ndraw,nomit,prior); % MCMC estimates 
res0.tflag = 'tstat';                       % based on non-truncated data
prt(res0);

results = sdmt_g(y,x,W,ndraw,nomit,prior);
results.tflag = 'tstat';
prt(results);
subplot(1,1,1);
tt=1:n;
plot(tt,ysave,tt,results.yhat,'--');
title('actual y vs predicted');
pause;

plot(tt,ysave,tt,results.ymean,'--');
title('actual y vs mean of latent y-draws');

