% PURPOSE: An example of using sac() on a small dataset
%          general spatial model
%                              
%---------------------------------------------------
% USAGE: sac_d3
%---------------------------------------------------

clear all;

% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
n = length(anselin);
randn('seed',202020);
x = randn(n,3);
xc = anselin(:,4);
yc = anselin(:,5);
vnames = strvcat('crime','constant','income','hvalue');
[j1 W j2] = xy2cont(xc,yc);

% create a nearest neighbor weight matrix for the error spatial autocorrelation
W2 = make_neighborsw(xc,yc,2);

% do Monte Carlo generation of an SAC model
sige = 10; 
evec = randn(n,1)*sqrt(sige);
beta = ones(3,1);
rho = 0.8; lam = -0.4; 
A = eye(n) - rho*W;  AI = inv(A);
B = eye(n) - lam*W2; BI = inv(B);
y = AI*x*beta + AI*BI*evec; % generate some data

res = sac(y,x,W,W2);
% print the output with variable names
prt(res,vnames);
plt(res);

% use same W-matrix for both spatial lag and spatial error
res2 = sac(y,x,W,W);
% print the output with variable names
prt(res2,vnames);


