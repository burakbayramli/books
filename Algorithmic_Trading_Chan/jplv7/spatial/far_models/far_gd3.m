% PURPOSE: An example of Metropolis-Hastings vs. sampling by inversion
%          using far_g
%---------------------------------------------------
% USAGE: far_gd3 
%---------------------------------------------------

clear all;

% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; % standardized 1st-order spatial weight matrix
xc = anselin(:,4);
yc = anselin(:,5);
W = make_neighborsw(xc,yc,5); % true model based on 5 nearest neighbors
[n junk] = size(W);
In = speye(n); 
randn('seed',1010);
rho = 0.7;  % true value of rho
sige = 0.1;
y = (In-rho*W)\(randn(n,1)*sqrt(sige)); 
ydev = y - mean(y);
vnames = strvcat('y-simulated','W*y');


ndraw = 2500;
nomit = 500;

% Gibbs sampling function homoscedastic prior
prior.novi = 1;   % homoscedastic prior for comparison to max like
prior.lflag = 0;  % full lndet calculation, no approximation
prior.dflag = 0;  % Sampling for rho by inversion


res1 = far_g(ydev,W,ndraw,nomit,prior);
prt(res1,vnames);

prior.dflag = 1;  % Metropolis-Hasting sampling for rho
res2 = far_g(ydev,W,ndraw,nomit,prior);
prt(res2,vnames);

% compare posterior densities from two types of sampling for rho
[h1,f1,y1] = pltdens(res1.pdraw);
[h2,f2,y2] = pltdens(res2.pdraw);

plot(y1,f1,'--r',y2,f2,'-g');
legend('inversion sampling','M-H sampling');
