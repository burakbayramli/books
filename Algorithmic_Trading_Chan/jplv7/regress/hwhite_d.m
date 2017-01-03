% PURPOSE: An example of  hwhite(),
%                         prt_reg(),
% Halbert White's heteroscedastic consistent estimator
%---------------------------------------------------
% USAGE: hwhite_d
%---------------------------------------------------


rand('seed',10);
n = 100; k=3;
xtmp = randn(n,k-1);

tt = 1:n;
ttp = tt';

e = randn(n,1).*ttp; % heteroscedastic error term

%e = randn(n,1);     % homoscedastic error term

b = ones(k,1);

iota = ones(n,1);

x = [iota xtmp];

% generate y-data
y = x*b + e;

vnames=['yvar',
        'iota',
        'x1  ',
        'x2  '];

% do ols regression
reso = ols(y,x);
prt_reg(reso,vnames);

% compare to Halbert White's regression
res = hwhite(y,x);
prt_reg(res,vnames);


