% PURPOSE: demo of ols_g() 
%          Gibbs sampling for Bayesian Heteroscedastic 
%          Linear Model
% 
%---------------------------------------------------
% USAGE: ols_gd
%---------------------------------------------------
clear all;
n=100; k=3; % set number of observations and variables
sige = 1;
randn('seed',10101);
x = randn(n,k); b = ones(k,1); % generate data set
tt = ones(n,1); tt(51:100,1) = [1:50]';
randn('seed',20201);
y = x*b + randn(n,1).*sqrt(tt); % heteroscedastic model
%y = x*b + randn(n,1)*sqrt(sige);  % homoscedastic model
ndraw = 11000; nomit = 100; % set the number of draws   

bmean = zeros(k,1);    % diffuse prior b means
T = eye(k)*1000;     % diffuse prior b variance 
rval = 4;                   % heteroscedastic prior
mm=8;                       % informative prior for r-value
kk=2;  
prior.beta = bmean;
prior.bcov = T;
%prior.m = mm;         % use proper prior on r-value
%prior.k = kk;
prior.rval = rval;     % use improper prior on r-value

result = ols_g(y,x,ndraw,nomit,prior);

prt(result);

subplot(3,1,1),
pltdens(result.bdraw(:,1));
xlabel('beta 1 posterior distribution');
subplot(3,1,2),
pltdens(result.bdraw(:,2));
xlabel('beta 2 posterior distribution');
subplot(3,1,3),
pltdens(result.bdraw(:,3));
xlabel('beta 3 posterior distribution');
pause;
subplot(1,1,1);
plot(result.vmean);
title('posterior mean of vi draws');
pause;
pltdens(result.sdraw);
title('posterior density for sige estimate');
pause;
