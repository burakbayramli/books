% PURPOSE: An example of using sem2_gmm
% GM estimation of spatial error model
% with 2 spatial weight matrices (on a small data set)  
%                                   
%---------------------------------------------------
% USAGE: sem2_gmmd (see also sem_gmmd2 for a large data set)
%---------------------------------------------------

clear all;
% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; 
xc = anselin(:,4);
yc = anselin(:,5);
% crate standardized 1st-order spatial weight matrix
[j1 W j2] = xy2cont(xc,yc);
M = make_neighborsw(xc,yc,1);
[n junk] = size(W);
IN = eye(n); 
rho1 = 0.6;  % true value of rho1
rho2 = 0.3;  % true value for rho2
sige = 0.5;
k = 3;
x = randn(n,k);
beta = zeros(k,1);
beta(1,1) = 1.0;
beta(2,1) = -1.0;
beta(3,1) = 1.0;

u = (IN - rho1*W - rho2*M)\(randn(n,1)*sqrt(sige));
y = x*beta + u;

results0 = sem(y,x,W);
prt(results0);

results1 = sem2_gmm(y,x,W,M);
prt(results1);

% turn on iteration
options.iter = 1;
results2 = sem2_gmm(y,x,W,M,options);
prt(results2);


