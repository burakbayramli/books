% PURPOSE: An example of logit(),
%                        prt_reg().
%  maximum likelihood estimation
%  (data from Mendenhall et. al 1989) 
%---------------------------------------------------
% USAGE: logit_d
%---------------------------------------------------

n = 24;
y = zeros(n,1);
y(1:14,1) = ones(14,1);

xdata = [21 24 25 26 28 31 33 34 35 37 43 49 ...
         51 55 25 29 43 44 46 46 51 55 56 58];
         
iota = ones(n,1);

x = [iota xdata'];      

vnames=['days    ',
        'iota    ',
        'response'];

reso = ols(y,x);
prt_reg(reso,vnames);

res = logit(y,x);
prt_reg(res,vnames);


