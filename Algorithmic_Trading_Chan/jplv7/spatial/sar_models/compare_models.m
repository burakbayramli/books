% PURPOSE: An example of model comparison using log marginal posterior
%          (on a large data set)                  
%---------------------------------------------------
% USAGE: compare_models
%---------------------------------------------------

clear all;

% NOTE a large data set with 3107 observations
% from Pace and Barry, takes around 150-250 seconds
load elect.dat;             % load data on votes
y =  log(elect(:,7)./elect(:,8));
x1 = log(elect(:,9)./elect(:,8));
x2 = log(elect(:,10)./elect(:,8));
x3 = log(elect(:,11)./elect(:,8));
latt = elect(:,5);
long = elect(:,6);
n = length(y); 
x = [ones(n,1) x1 x2 x3];
clear x1; clear x2; clear x3;
clear elect;                % conserve on RAM memory
[n,k] = size(x);
[junk W junk] = xy2cont(latt,long);
vnames = strvcat('voters','const','educ','homeowners','income');

% generate sem model
IN = speye(n);
beta = ones(k,1);
rho = 0.8;
sige = 1;

y = x*beta + (IN - rho*W)\(randn(n,1)*sqrt(sige));

% Gibbs sampling function homoscedastic prior
prior.novi = 1;  % homoscedastic prior for comparison
ndraw = 2000;
nomit = 500;

results1 = sem_g(y,x,W,ndraw,nomit,prior);

results2 = sar_g(y,x,W,ndraw,nomit,prior);

probs = model_probs(results1,results2);

fprintf(1,'posterior probs for sar versus sem model \n');
fprintf(1,'*** true model is sem ***  \n');

in.rnames = strvcat('Models','sem','sar');
mprint(probs,in);


% generate an sar model     
% sar model generated here
y = (IN-rho*W)\(x*beta) + (IN-rho*W)\(randn(n,1)*sqrt(sige)); 

% homoscedastic prior
prior2.novi = 1;
ndraw = 2000;
nomit = 500;

results3 = sem_g(y,x,W,ndraw,nomit,prior2);

results4 = sar_g(y,x,W,ndraw,nomit,prior2);


probs = model_probs(results3,results4);

fprintf(1,'posterior probs for sar versus sem model \n');
fprintf(1,'*** true model is sar ***  \n');
in.rnames = strvcat('Models','sem','sar');
mprint(probs,in);


