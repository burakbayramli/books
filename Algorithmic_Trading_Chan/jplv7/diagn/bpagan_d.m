% PURPOSE: An example of  bpagan(),
%
% Breush-Pagan heteroscedasticity test
%---------------------------------------------------
% USAGE: bpagan_d
%---------------------------------------------------

n = 100; k=5;
x = randn(n,k);
x(:,1) = ones(n,1);
b = ones(k,1);
y = x*b + randn(n,1);

% print output to command window
bpagan(y,x);

tt=1:n; ttp = tt';
x = randn(n,k);
x(:,1) = ones(n,1);
e = randn(n,1).*ttp;
y2 = x*b + e;
% return output to results structure
result = bpagan(y2,x);
result
