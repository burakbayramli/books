% PURPOSE: An example of using sar_g() sem_g() Gibbs sampling
%          spatial model comparisons using log marginal posterior
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: model_compare
%---------------------------------------------------

clear all;

load anselin.dat; % standardized 1st-order spatial weight matrix
y = anselin(:,1);
n = length(y);
x = [ones(n,1) anselin(:,2:3)];
[n,k] = size(x);
vnames = strvcat('crime','constant','income','hvalue');

load wmat.dat;
W = sparse(wmat(:,1),wmat(:,2),wmat(:,3));

[n junk] = size(W);


% Gibbs sampling function homoscedastic prior
prior.novi = 1;  % homoscedastic prior for comparison
ndraw = 2500;
nomit = 500;
prior.lflag = 0; % full lndet calculation

results1 = sem_g(y,x,W,ndraw,nomit,prior);
prt(results1);  
results2 = sdm_g(y,x,W,ndraw,nomit,prior);
prt(results2);  
probs = model_probs(results1,results2);

fprintf(1,'posterior probs for sem versus sdm model \n');

in.rnames = strvcat('Models','sem','sdm');
mprint(probs,in);

% do maximum likelihood estimates for comparison
info.lflag = 0; % full lndet calculation
results3 = sem(y,x,W,info);
prt(results3);  
results4 = sdm(y,x,W,info);
prt(results4);  

