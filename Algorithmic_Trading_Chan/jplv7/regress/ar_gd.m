% PURPOSE: An example using ar_g(),
%                           prt(),
%                           plt(),
% Bayesian gibbs sampling for the ar model
%---------------------------------------------------
% USAGE: ar_gd
%---------------------------------------------------

n = 200; nobs = n;
k = 3;
e = randn(n,1)*10;
y = zeros(n,1);
for i=3:n
    y(i,1) = 1 + y(i-1,1)*0.25 + y(i-2,1)*0.75 + e(i,1);
end;


yt = y(101:n,1);
x = [ones(nobs,1) mlag(y,2)];
xt = trimr(x,100,0);
nobse = rows(yt);

vnames = strvcat('y-variable','constant','ylag1','ylag2');

res = ols(yt,xt);
prt_reg(res,vnames);

ndraw = 1100;
nomit = 100;

% prior parameters
pvar = eye(k)*100;  % diffuse prior variance
pmean = zeros(k,1); % diffuse prior means
rval = 4;           % prior for heteroscedasticity

prior.beta = pmean;
prior.bcov = pvar;
prior.rval = rval;
prior.rmat = eye(k);
nlag = 2;

result = ar_g(yt,nlag,ndraw,nomit,prior);
result.pflag = 'tstat';
prt(result,'y-variable');
plt(result);
pause;

res = theil(yt,xt,pmean,eye(3),pvar);
prt(res,vnames);
plt(res);