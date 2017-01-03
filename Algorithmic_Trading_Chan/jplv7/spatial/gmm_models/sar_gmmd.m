% PURPOSE: An example of using sar_gmm
% GM estimation of spatial autoregressive model (on a small data set)  
%                                   
%---------------------------------------------------
% USAGE: sar_gmmd (see also sar_gmmd2 for a large data set)
%---------------------------------------------------

clear all;
% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; 
xc = anselin(:,4);
yc = anselin(:,5);
% crate standardized 1st-order spatial weight matrix
[j1 W j2] = xy2cont(xc,yc);
[n junk] = size(W);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 0.5;
k = 3;
x = randn(n,k);
beta = zeros(k,1);
beta(1,1) = 1.0;
beta(2,1) = -1.0;
beta(3,1) = 1.0;

A = (IN - rho*W);
y = A\x*beta + A\(randn(n,1)*sqrt(sige));

results0 = sar(y,x,W);
prt(results0);

results1 = sar_gmm(y,x,W);
prt_gmm(results1);
