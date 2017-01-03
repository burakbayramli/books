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
vnames = strvcat('y','var1','var2','var3','var4');

x = studentize(x);

b = ones(k,1);
rho = -0.67;
sige = 2.5;
y = (speye(n) - rho*W)\(x*b) + (speye(n) - rho*W)\(randn(n,1)*sqrt(sige));


% use defaults including lndet approximation
result = sar(y,x,W); % maximum likelihood estimates
prt(result,vnames);

logL_sar = result.lik;


resulto = ols(y,x);
prt(resulto,vnames);

% compute ols log-likelihood
epe = resulto.resid'*resulto.resid;
nn = resulto.nobs;
nk = resulto.nvar;

sige = epe/(nn-nk);

term1 = -(nn/2)*(log(2*pi) + log(sige));
e = resulto.resid;
term2 = 0;
for i=1:nn;
term2 = term2  + (1/sige)*e(i,1)*e(i,1);
end;
term2 = -0.5*term2;

logL_ols = term1 + term2;

result2 = sem(y,x,W);
prt(result2,vnames);
logL_sem = result2.lik;


[logL_ols logL_sem logL_sar]
