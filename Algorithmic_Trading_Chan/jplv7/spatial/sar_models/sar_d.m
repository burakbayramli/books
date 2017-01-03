% PURPOSE: An example of using sar() 
%          spatial autoregressive model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: sar_d (see also sar_d2 for a large data set)
%---------------------------------------------------

clear all;

% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; % standardized 1st-order spatial weight matrix
latt = anselin(:,4);
long = anselin(:,5);
[junk W junk] = xy2cont(latt,long);
[n junk] = size(W);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 0.1;
k = 3;
x = [ones(n,1) randn(n,k) ];
betav = zeros(k+1,1);
betav(1,1) = 10.0;
betav(2,1) = -1.0;
betav(3,1) = 1.0;
betav(4,1) = 2.0;

y = inv(IN-rho*W)*x*betav + (IN-rho*W)\randn(n,1)*sqrt(sige); 

info.lflag = 0; % use full lndet no approximation
result0 = sar(y,x,W,info);
prt(result0);

result1 = sar(y,x,W); % default to Barry-Pace lndet approximation
prt(result1);
plt(result1);
