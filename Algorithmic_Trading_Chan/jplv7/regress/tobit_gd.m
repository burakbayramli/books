% PURPOSE: An example using tobit_g()
%          Gibbs sampling tobit censored regression
%                         
%---------------------------------------------------
% USAGE: tobit_gd
%---------------------------------------------------

clear all;
% generate uncensored data
n=100; k = 2; sige = 5;
evec = randn(n,1)*sqrt(sige);
tt=1:n;
x = randn(n,k);
x(1:n,1) = ones(n,1);

b = ones(k,1);
b(2,1) = 2.0;
b(1,1) = 0.5;

y = x*b + evec;
% add a few outliers
y(50,1) = 15;
y(75,1) = 15;

yc = zeros(n,1);

% now censor the data
ind = find(y >= 0);
yc(ind,1) = y(ind,1);
if length(ind) == n
	error('no censored observations -- try again');
end;

Vnames = strvcat('y','constant','x1');
          
res1 = ols(yc,x);
res2 = tobit(yc,x);

prior.rval = 40;        % heteroscedastic prior
ndraw = 1100;
nomit = 100;
result = tobit_g(yc,x,ndraw,nomit,prior);

plot(tt,result.vmean);
title('vi-estimates');

prt(res1,Vnames);
prt(res2,Vnames);
prt(result,Vnames);


