% PURPOSE: An example using tsls(),
%                           prt_reg()
% Two-stage Least-squares
%                              
%---------------------------------------------------
% USAGE: tsls_d
%---------------------------------------------------

clear all;

nobs = 200;

x1 = randn(nobs,1);
x2 = randn(nobs,1);
b1 = 1.0;
b2 = 1.0;
iota = ones(nobs,1);

y1 = zeros(nobs,1);
y2 = zeros(nobs,1);
evec = randn(nobs,1);

% create simultaneously determined variables y1,y2
for i=1:nobs;
y1(i,1) = iota(i,1)*1.0 + x1(i,1)*b1 + evec(i,1);
y2(i,1) = iota(i,1)*1.0 + y1(i,1)*1.0 + x2(i,1)*b2 + evec(i,1);
end;


vname1 = ['y1-eqn  ',
          'y2 var  ',
          'constant',          
          'x1 var  '];
          
vname2 = ['y2-eqn  ',
          'y1 var  ',
          'constant',
          'x2 var  '];
          
% use all exogenous in the system as instruments
xall = [iota x1 x2];            

% do ols regression
result1 = ols(y2,[y1 iota x2]);
prt_reg(result1,vname2);

% do tsls regression
result2 = tsls(y2,y1,[iota x2],xall); 
prt_reg(result2,vname2);

% do Monte Carlo looping
niter = 100;

bols = result1.beta;
b2sls = result2.beta;

disp('patience -- doing 100 2sls regressions');

for iter=1:niter;

y1 = zeros(nobs,1);
y2 = zeros(nobs,1);
evec = randn(nobs,1);

% create simultaneously determined variables y1,y2
for i=1:nobs;
y1(i,1) = iota(i,1)*1.0 + x1(i,1)*b1 + evec(i,1);
y2(i,1) = iota(i,1)*1.0 + y1(i,1)*1.0 + x2(i,1)*b2 + evec(i,1);
end;

% do ols regression
result1 = ols(y2,[y1 iota x2]);

% do tsls regression
result2 = tsls(y2,y1,[iota x2],xall); 

bols = bols + result1.beta;
b2sls = b2sls + result2.beta;

end;

% find averages over the niter runs
bols = bols*(1/niter);
b2sls = b2sls*(1/niter);

% print results based on averages
fprintf(['average OLS results over ',num2str(niter),' runs\n']);
mprint(bols);

fprintf(['average TSLS results over ',num2str(niter),' runs\n']);
mprint(b2sls);


