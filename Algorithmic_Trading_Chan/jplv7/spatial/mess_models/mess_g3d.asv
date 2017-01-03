% PURPOSE: An example of using mess_g3() on a small dataset
% Gibbs sampling the matrix exponential spatial model
% using Anselin's Columbus crime data
% this function samples over both rho and #neighbors
% to produce a joint posterior for all parameters in the model
%                              
%---------------------------------------------------
% USAGE: mess_g3d
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
ndraw = 2500;
nomit = 500;
option.latt = xc;
option.long = yc;
option.mmin = 1;
option.mmax = 10;
option.rmin = .5;
option.rmax = 1;

res1 = mess_g3(y,x,option,ndraw,nomit);
prt(res1,vnames);

hist(res1.adraw);
title('posterior distribution for alpha');
pause;
hist(res1.rdraw);
title('posterior distribution for rho');
pause;
hist(res1.mdraw);
title('posterior distribution for neigh');
pause;

option.xflag = 1;
res2 = mess_g3(y,x,option,ndraw,nomit);
prt(res2,vnames);



