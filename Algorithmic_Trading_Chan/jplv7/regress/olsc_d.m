% PURPOSE: demonstrate ols_corc roc 
%
% USAGE: olsc_d
% -----------------------------------------------

% generate a model with 1st order serial correlation
n = 200;
k = 3;
tt = 1:n;
evec = randn(n,1);
xmat = randn(n,k);
xmat(:,1) = ones(n,1);
beta = ones(k,1);
beta(1,1) = 10.0; % constant term
y = zeros(n,1);
u = zeros(n,1);

for i=2:n;
 u(i,1) = 0.4*u(i-1,1) + evec(i,1);
 y(i,1) = xmat(i,:)*beta + u(i,1);
end;

% truncate 1st 100 observations for startup
yt = y(101:n,1);
xt = xmat(101:n,:);
n = n-100; % reset n to reflect truncation
 
Vnames = ['y    ',
          'cterm',
          'x2   ',
          'x3   '];
          
% do ols regression
result = ols(yt,xt);
prt_reg(result,Vnames);
plt_reg(result);
pause;
% do Cochrane-Orcutt regression
result2 = olsc(yt,xt);
prt_reg(result2,Vnames);
plt_reg(result2);


