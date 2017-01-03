% PURPOSE: demo of probit_g
%       Gibbs sampling for probit heteroscedastic estimation
%---------------------------------------------------
% USAGE: probit_gd
%---------------------------------------------------

clear all;

n=100;
k = 3;
evec = randn(n,1);
tt=1:n;

x = randn(n,k);
x(1:n,1) = ones(n,1);

b = ones(k,1);
b(3,1) = -2.0;
b(2,1) = 2.0;
b(1,1) = -0.5;

y = x*b + 0.2*evec;
yc = zeros(n,1);
% now censor the data
for i=1:n
 if y(i,1) > 0
 yc(i,1) = 1;
 else
 yc(i,1) = 0;
 end;
end;

% add outliers
%x(50,2) = 5;
%x(75,3) = 5;

Vnames = strvcat('y','constant','x1','x2');

prior.rval = 40;     % heteroscedastic prior
ndraw = 1100;
nomit = 100;

result = probit_g(yc,x,ndraw,nomit,prior);

plot(tt,result.vmean);
title('vi-estimates');
pause;

prt(result,Vnames);

tt=1:n;
[ys yi] = sort(result.y);
plot(tt,ys,tt,result.yhat(yi,1),'--');

plot(tt,result.ymean,tt,y,'o');
