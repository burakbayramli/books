% PURPOSE: An example using nwest(),
%                           prt_reg(),
% Newey-West hetero/serial consistent estimates
%---------------------------------------------------
% USAGE: nwest_d
%---------------------------------------------------


rand('seed',10);
n = 100; k=3;
xtmp = randn(n,k-1)*10;

tt = 1:n;
ttp = tt';

e = randn(n,1).*sqrt(ttp); % heteroscedastic error term

u = zeros(n,1);% serial heteroscedastic error term
u(1,1) = e(1,1);
for i=2:n
u(i,1) = 0.8*u(i-1,1) + e(i,1);
end;

 

b = ones(k,1);

iota = ones(n,1);

x = [iota xtmp];

% generate y-data
y = x*b + u;

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

% compare to Newey-West regression
nlag=2;
res = nwest(y,x,nlag);
prt_reg(res,vnames);


