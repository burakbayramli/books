% PURPOSE: An example using tobit()
%          with right-censoring
%  Tobit (censored) regression                           
%---------------------------------------------------
% USAGE: tobit_d2
%---------------------------------------------------

clear all;

% generate uncensored data
n=100; k=5;
x = randn(n,k);
x(:,1) = ones(n,1);
beta = -ones(k,1)*2.0;

y = x*beta + randn(n,1);

% now censor the data
[ys yi] = sort(y);
xs = x(yi,:);
limit = ys(80,1);
for i=80:n
 ys(i,1) = limit;
end;

vnames = strvcat('y','iota','x1','x2','x3','x4');        

info.trunc = 'right';
info.limit = limit;
tic; resp = tobit(ys,xs,info); toc;      
prt(resp,vnames);
plt(resp);
pause;
reso = ols(ys,xs);
prt(reso,vnames);
plt(reso);
