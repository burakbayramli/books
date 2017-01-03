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

latt = anselin(:,4);
long = anselin(:,5);
[junk W junk] = xy2cont(latt,long);
[n junk] = size(W);

% generate sem model
IN = eye(n);
beta = ones(k,1);
x = randn(n,k);
rho = 0.7;
sige = 1;

y = x*beta + (IN - rho*W)\(randn(n,1)*sqrt(sige));

% Gibbs sampling function homoscedastic prior
prior.novi = 1;  % homoscedastic prior for comparison
ndraw = 2500;
nomit = 500;

results1 = sem_g(y,x,W,ndraw,nomit,prior);
prt(results1);  
results2 = sar_g(y,x,W,ndraw,nomit,prior);
prt(results2);  
probs = model_probs(results1,results2);

fprintf(1,'posterior probs for sar versus sem model \n');
fprintf(1,'*** true model is sem ***  \n');

in.rnames = strvcat('Models','sem','sar');
mprint(probs,in);


% generate an sar model 
n = length(latt);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 0.5;
k = 3;
x = randn(n,k);
x(:,1) = ones(n,1);
beta(1,1) = -1.0;
beta(2,1) = 0.0;
beta(3,1) = 1.0;

% for sar model
vnames = strvcat('y','constant','x1','x2');
    
% sar model generated here
y = (IN-rho*W)\(x*beta) + (IN-rho*W)\(randn(n,1)*sqrt(sige)); 

% homoscedastic prior
prior2.novi = 1;
ndraw = 2500;
nomit = 500;

results3 = sem_g(y,x,W,ndraw,nomit,prior2);
prt(results3); 
results4 = sar_g(y,x,W,ndraw,nomit,prior2);
prt(results4);  
probs = model_probs(results3,results4);

fprintf(1,'posterior probs for sar versus sem model \n');
fprintf(1,'*** true model is sar ***  \n');
in.rnames = strvcat('Models','sem','sar');
mprint(probs,in);


