% PURPOSE: demo of norm_prb function
%          used to compute marginal probabilities
%          for the case of asymptotic t-ratios
%
%---------------------------------------------------
% USAGE: norm_prbd
%---------------------------------------------------

nobs = 1000; nvar = 3;
x = randn(nobs,nvar);
b = ones(nvar,1);
y = x*b + 100*randn(nobs,1);

res = ols(y,x);
prt(res);

tstat = res.tstat;
nprob = norm_prb(tstat);
tprob = tdis_prb(tstat,nobs-nvar);
in.cnames = strvcat('t-probability','normal-probability');
mprint([tprob nprob],in);
