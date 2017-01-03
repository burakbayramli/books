% PURPOSE: An example using ols(),
%                           prt(),
%                           plt(),
% ordinary least-squares estimation
%---------------------------------------------------
% USAGE: ols_d
%---------------------------------------------------

nobs = 1000;
nvar = 15;
beta = ones(nvar,1);

xmat = randn(nobs,nvar-1);

x = [ones(nobs,1) xmat];
evec = randn(nobs,1);

y = x*beta + evec;

vnames = strvcat('y-vector','constant','x1',';x2','x3','x4','x5','x6', ...
                 'x7','x8','x9','x10','x11','x12','x13','x14');

% do ols regression
result = ols(y,x); 

% print the output
prt(result,vnames);

% plot the predicted and residuals
plt(result);
pause;

% recover residuals
resid = result.resid;

% print out tstats
fprintf(1,'tstatistics = \n');
result.tstat

% plot actual vs. predicted
tt=1:nobs;
plot(tt,result.y,tt,result.yhat,'--');



