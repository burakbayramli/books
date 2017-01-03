% PURPOSE: An example of using sar_gmm on a simulated large data set   
%          GM estimation of the spatial autoregressive model                         
%---------------------------------------------------
% USAGE: sar_gmmd3 (see sar_gmmd for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 150-250 seconds
load elect.dat;                    % load data on votes
latt = elect(:,5);
long = elect(:,6);
n = length(latt);
k = 4;
x = randn(n,k);
clear elect;                % conserve on RAM memory

[j,W,j] = xy2cont(latt,long); % contiguity-based spatial Weight matrix

rho = 0.7;
beta = ones(k,1);
sige = 0.5;

B = speye(n) - rho*W;
y = B\(x*beta) + B\(randn(n,1)*sqrt(sige));

% use defaults including lndet approximation
result = sar(y,x,W); % maximum likelihood estimates
prt(result);

result2 = sar_gmm(y,x,W);
prt_gmm(result2);



