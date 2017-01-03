% PURPOSE: An example of using sdm_g() sem_g() Gibbs sampling
%          spatial model comparisons using log marginal posterior
%          (on a small data set)                  
%---------------------------------------------------
% USAGE: model_compare
%---------------------------------------------------

clear all;

load anselin.dat; % standardized 1st-order spatial weight matrix
y = anselin(:,1);
n = length(y);

latt = anselin(:,4);
long = anselin(:,5);
[junk W junk] = xy2cont(latt,long);
[n junk] = size(W);

% generate sem model
IN = eye(n);
k = 3;
beta = ones(k,1);
rho = 0.7;
sige = 0.1;
x = randn(n,k);
x(:,1) = ones(n,1);

y = x*beta + (IN - rho*W)\(randn(n,1)*sqrt(sige));

clear prior;
prior.novi=1;    % homooscedastic prior

ndraw = 1200;
nomit = 200;

results1 = sem_g(y,x,W,ndraw,nomit,prior);

results2 = sdm_g(y,x,W,ndraw,nomit,prior);

probs = model_probs(results1,results2);

fprintf(1,'posterior probs for sdm versus sem model \n');
fprintf(1,'*** true model is sem ***  \n');

in.rnames = strvcat('Models','sem','sdm');
mprint(probs,in);


% generate an sdm model 
n = length(latt);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 10;
k = 3;
x = randn(n,k);
x(:,1) = ones(n,1);
beta(1,1) = -1.0;
beta(2,1) = 0.0;
beta(3,1) = 1.0;

% for sdm model
Wx = W*x(:,2:end);
xsdm = [x Wx];
beta = [beta
        beta(2:end,1)];

    
% sdm model generated here
y = (IN-rho*W)\(xsdm*beta) + (IN-rho*W)\(randn(n,1)*sqrt(sige)); 

clear prior2;
prior2.novi = 1;   % homoscedastic model
ndraw = 1200;
nomit = 200;

results3 = sem_g(y,x,W,ndraw,nomit,prior2);

results4 = sdm_g(y,x,W,ndraw,nomit,prior2);

probs = model_probs(results3,results4);

fprintf(1,'posterior probs for sdm versus sem model \n');
fprintf(1,'*** true model is sdm ***  \n');
in.rnames = strvcat('Models','sem','sdm');
mprint(probs,in);



% sdm model generated here
y = (IN-rho*W)\(xsdm*beta) + (IN-rho*W)\(randn(n,1)*sqrt(sige)); 

clear prior2;
prior2.novi = 1;   % homoscedastic model
ndraw = 1200;
nomit = 200;

results3 = sar_g(y,x,W,ndraw,nomit,prior2);

results4 = sdm_g(y,x,W,ndraw,nomit,prior2);

probs = model_probs(results3,results4);

fprintf(1,'posterior probs for sdm versus sar model \n');
fprintf(1,'*** true model is sdm ***  \n');
in.rnames = strvcat('Models','sar','sdm');
mprint(probs,in);


% generate sar model
IN = eye(n);
k = 3;
beta = ones(k,1);
rho = 0.7;
sige = 0.1;
x = randn(n,k);
x(:,1) = ones(n,1);

y = (IN - rho*W)\x*beta + (IN - rho*W)\(randn(n,1)*sqrt(sige));

clear prior2;
prior2.novi = 1;   % homoscedastic model
ndraw = 1200;
nomit = 200;

results3 = sar_g(y,x,W,ndraw,nomit,prior2);

results4 = sdm_g(y,x,W,ndraw,nomit,prior2);

probs = model_probs(results3,results4);

fprintf(1,'posterior probs for sar versus sdm model \n');
fprintf(1,'*** true model is sar ***  \n');
in.rnames = strvcat('Models','sar','sdm');
mprint(probs,in);


