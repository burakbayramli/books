% PURPOSE: demo of recresid()
%          Recursive residuals
% 
%---------------------------------------------------
% USAGE: recresid_d
%---------------------------------------------------

n = 100; k = 5;
b = ones(k,1);
x = randn(n,k);
x(:,1) = ones(n,1);
y = x*b + randn(n,1);

rresid = recresid(y,x);


% generate structural shift
b2 = ones(k,1)*3;
xb = x(n/2+1:n,:)*b2;
y(n/2+1:n,1) = xb + randn(n/2,1);

rresid2 = recresid(y,x);

tt=1:n;
subplot(2,1,1),
plot(tt,rresid)
title('Recursive residuals good model');
subplot(2,1,2),
plot(tt,rresid2);
title('Recursive residuals with shift in regime');
