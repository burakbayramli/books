% PURPOSE: An example of using sar_g() Gibbs sampling
%          spatial autoregressive model
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: sar_gd (see also sar_gd2 for a large data set)
%---------------------------------------------------

clear all;


% load Anselin (1988) Columbus neighborhood crime data
load anselin.dat; 
% 5 columns:
% column1 = crime
% column2 = household income
% column3 = house values
% column4 = latitude coordinate
% column5 = longitude coordinate

n = length(anselin);
y = anselin(:,1);
x = [ones(n,1) anselin(:,2:3)]; 
latt = anselin(:,4);
long = anselin(:,5);
vnames = strvcat('crime','constant','income','hvalue');

% 1st order contiguity matrix for
% Anselin's Columbus crime dataset
% stored in sparse matrix format [i, j, s] = find(W);
% so that W = sparse(i,j,s); reconstructs the 49x49 matrix
% NOTE: already row-standardized

load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));


info.lflag = 0;
result0 = sar(y,x,W,info);
prt(result0,vnames);

ndraw = 2500;
nomit = 500;

prior2.novi = 1; % homoscedastic model
% uses default numerical integration for rho
prior2.lflag = 0;
results2 = sar_g(y,x,W,ndraw,nomit,prior2);
results2.tflag = 'tstat';
prt(results2,vnames);

% Gibbs sampling function heteroscedastic prior
% to maximum likelihood estimates
prior.rval = 4;
prior.lflag = 0;
results = sar_g(y,x,W,ndraw,nomit,prior);
results.tflag = 'tstat';
prt(results,vnames);


[h1,f1,y1] = pltdens(results.pdraw);
[h2,f2,y2] = pltdens(results2.pdraw);
plot(y1,f1,'.r',y2,f2,'.g');
legend('heteroscedastic','homoscedastic');
title('posterior distributions for rho');
xlabel('rho values');

probs = model_probs(results,results2);

fprintf(1,'posterior model probabilities \n');
in2.rnames = strvcat('Models','heteroscedastic model','homoscedastic model');
in2.cnames = strvcat('Posterior model probabilities');
in2.fmt = '%12.6f';

mprint(probs,in2);

