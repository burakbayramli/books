% PURPOSE: An example of using far_g() Gibbs sampling
%          1st-order spatial autoregressive model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: far_gd (see also far_gd2 for a large data set)
%---------------------------------------------------

clear all;

% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; % standardized 1st-order spatial weight matrix
xc = anselin(:,4);
yc = anselin(:,5);
[j1 W j2] = xy2cont(xc,yc);
[n junk] = size(W);
In = speye(n); 
rho = 0.7;  % true value of rho
sige = 1;
y = (In-rho*W)\(randn(n,1)*sqrt(sige)); 
ydev = y - mean(y);
vnames = strvcat('y-simulated','y-spatial lag');

% do maximum likelihood for comparison    
info.rmin = 0; 
info.rmax = 1; % constrain 0 < rho < 1     
result1 = far(ydev,W,info);
disp('True value of rho = 0.7');
prt(result1,vnames);

ndraw = 2200;
nomit = 200;

% Gibbs sampling function homoscedastic prior
prior.rval = 200; % homoscedastic prior for comparison
                  % to maximum likelihood estimates
result2 = far_g(ydev,W,ndraw,nomit,prior);
disp('True value of rho = 0.7');
result.tflag = 'tstat';
prt(result2,vnames);

plt(result2);