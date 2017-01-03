% PURPOSE: An example using tobit()
%                           prt()
%  Tobit (censored) regression                           
%---------------------------------------------------
% USAGE: tobit_d
%---------------------------------------------------

clear all;
rand('state',1);
% generate uncensored data
n=100; k=5;
x = randn(n,k);
x(:,1) = ones(n,1);
beta = ones(k,1)*2.0;
beta(1,1) = -2.0;
beta(2,1) = -2.0;

y = x*beta + randn(n,1);

% now censor the data
for i=1:n
 if y(i,1) < 0
 y(i,1) = 0.0;
 end;
end;

vnames = strvcat('y','iota','x1','x2','x3','x4');        
            
resp = tobit(y,x);     
prt(resp,vnames);
info.meth = 'dfp';
resp = tobit(y,x,info);
prt(resp,vnames);
