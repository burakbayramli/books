% PURPOSE: An example of using mess() on a small dataset
%          matrix exponential spatial specification
%                              
%---------------------------------------------------
% USAGE: mess_d
%---------------------------------------------------

% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
n = length(anselin);
x = [ones(n,1) anselin(:,2:3)]; 
latt = anselin(:,4);
long = anselin(:,5);
vnames = strvcat('crime','constant','income','hvalue');

load wmat.dat; 
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));;

% do Monte Carlo generation of an SAR model
sige = 100; 
evec = randn(n,1)*sqrt(sige);
beta = ones(3,1);
rho = 0.6; lam = 0.25; 
A = eye(n) - rho*W;  AI = inv(A);
y = AI*x*beta + AI*evec; % generate some data



% do MESS model using W as weight matrix
option.D = W;
res1 = mess(y,x,option);
prt(res1,vnames);

% do MESS model using latt, long and neighbors
% to form a weight matrix
option2.latt = latt;
option2.long = long;
option2.rho = 0.75;
option2.neigh = 7;
res2 = mess(y,x,option2);
prt(res2,vnames);


% do MESS model with spatially lagged x-variables
option3.latt = latt;
option3.long = long;
option3.rho = 0.9;
option3.neigh = 10;
option3.xflag = 1;
res3 = mess(y,x,option3);
prt(res3,vnames);

