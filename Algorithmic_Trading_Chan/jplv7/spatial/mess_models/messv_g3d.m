% PURPOSE: An example of using messv_g3() on a small dataset
% Gibbs sampling the matrix exponential heteroscedastic spatial model
% using Anselin's Columbus crime data
% this function samples over both rho and #neighbors
% to produce a joint posterior for all parameters in the model
%                              
%---------------------------------------------------
% USAGE: messv_g3d
%---------------------------------------------------

clear all;
% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat;
y = anselin(:,1);  n = length(y);
x = [ones(n,1) anselin(:,2:3)];
xc = anselin(:,4); % x-coordinates
yc = anselin(:,5); % y-coordinates

vnames = strvcat('crime','constant','income','hvalue');  


% do Bayesian MCMC estimate
ndraw = 1250;
nomit = 250;
option.latt = xc;
option.long = yc;
option.mmin = 1;
option.mmax = 10;
option.rmin = .1;
option.rmax = 1;

prior.rval = 4; % note this is the default
                % if you enter nothing by way of a prior

res1 = messv_g3(y,x,option,ndraw,nomit,prior);
prt(res1,vnames);

% plot the vi estimates
tt=1:n;
plot(tt,res1.vmean);
title('posterior mean of vi estimates');



