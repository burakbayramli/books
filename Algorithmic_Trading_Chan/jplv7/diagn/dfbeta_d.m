% PURPOSE: demo of dfbeta(), plt_dfb()
%          influential observations diagnostics
%          from Belsley, Kuh and Welsch (1980)
%          REGRESSION DIAGNOSTICS
%---------------------------------------------------
% USAGE: dbeta_d
%---------------------------------------------------

% generate data set
n = 100;
k = 3;
x = randn(n,k);
x(:,1) = ones(n,1);
x(:,3) = x(:,2) + randn(n,1)*0.05;

beta = ones(k,1);

y = x*beta + randn(n,1);

% now add a few outliers
y(50,1) = 10.0;
y(70,1) = -10.0;

result = dfbeta(y,x);

vnames = ['y-vector',
          'constant',
          'x1 var  ',
          'x2 var  ',
          'x3 var  '];

plt_dfb(result,vnames);
pause;


n = 100;
k = 12;
x = randn(n,k);
x(:,1) = ones(n,1);

beta = ones(k,1);

y = x*beta + randn(n,1);

% now add a few outliers
y(50,1) = 10.0;
y(70,1) = -10.0;

result = dfbeta(y,x);
plt_dfb(result);
pause;

% plot dffits, studentized residuals and hat-matrix diagonal
plt_dff(result);
