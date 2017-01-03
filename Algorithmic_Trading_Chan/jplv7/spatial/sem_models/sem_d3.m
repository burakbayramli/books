% PURPOSE: An example of using sem() on a large data set   
%          Gibbs sampling spatial autoregressive model                         
%---------------------------------------------------
% USAGE: sem_d2 (see sem_d for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 150-250 seconds
load elect.dat;                    % load data on votes
latt = elect(:,5);
long = elect(:,6);
n = length(long);

[j,W,j] = xy2cont(latt,long); % contiguity-based spatial Weight matrix

x = randn(n,4);
beta = ones(4,1);

rho = 0.7;

u = (speye(n) - rho*W)\randn(n,1);

y = x*beta + u;
vnames = strvcat('voters','x1','x2','x3','x4');

% use defaults including lndet approximation
result = sem(y,x,W); % maximum likelihood estimates
prt(result,vnames);

info.eigs = 0; % compute minimum and maximum eigenvalues of W 
               % to determine bounds on rho
info.lflag = 0; % use full log-determinant
result2 = sem(y,x,W,info); % maximum likelihood estimates
prt(result2,vnames);

