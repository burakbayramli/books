% PURPOSE: demo of random draws from a Wishart distribution
% 
%---------------------------------------------------
% USAGE: wish_d
%---------------------------------------------------

ndraws = 1000;
n = 100; k=5;
x = randn(n,5);
xpx = x'*x;
xpxi = inv(xpx);
v = 10;

w = zeros(k,k);
for i=1:ndraws;
w = w + wish_rnd(xpx,v);
end;

fprintf('mean of wishart should = \n');
mprint(v*xpx);

fprintf('mean of wishart draws = \n');
mprint(w/ndraws);


