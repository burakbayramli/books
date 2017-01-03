% PURPOSE: An example of using sar_g() on a large data set   
%          Gibbs sampling spatial autoregressive model                         
%---------------------------------------------------
% USAGE: sar_gd2 (see sar_gd for a small data set)
%---------------------------------------------------

clear all;
% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 150-250 seconds
load elect.dat;             % load data on votes
latt = elect(:,5);
long = elect(:,6);
n = length(latt); 
k = 4;
x = randn(n,k);
clear elect;                % conserve on RAM memory
n = 3107;
[junk W junk] = xy2cont(latt,long);
vnames = strvcat('voters','const','educ','homeowners','income');

b = ones(k,1);
rho = 0.67;
sige = 0.5;
y = (speye(n) - rho*W)\(x*b) + (speye(n) - rho*W)\(randn(n,1)*sqrt(sige));

% use defaults including lndet approximation
result = sar(y,x,W); % maximum likelihood estimates
prt(result,vnames);

% use no lndet approximation
info.lflag = 0;
result2 = sar(y,x,W,info); % maximum likelihood estimates
prt(result2,vnames);

