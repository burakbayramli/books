% PURPOSE: An example of using sem 
% Maximum likelihood spatial error model(on a small data set)  
%                                   
%---------------------------------------------------
% USAGE: sem_gcd (see also sem_gcd2 for a large data set)
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

u = (IN - rho*W)\(randn(n,1)*sqrt(sige));
y = x*beta + u;

info.lflag = 0; % use exact log-determinant
results0 = sem(y,x,W,info);
prt(results0);
