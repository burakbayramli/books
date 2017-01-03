% PURPOSE: An example of using priors with sar_g() Gibbs sampling
%          spatial autoregressive model (on a small data set)                  
%---------------------------------------------------
% USAGE: sar_gd4 (see also sar_gd2 for a large data set)
%---------------------------------------------------

clear all;

% W-matrix from Anselin's neigbhorhood crime data set
load anselin.dat; % standardized 1st-order spatial weight matrix
latt = anselin(:,4);
long = anselin(:,5);
[junk W junk] = xy2cont(latt,long);
[n junk] = size(W);
IN = eye(n); 
rho = 0.7;  % true value of rho
sige = 0.5;
k = 3;

randn('seed',10201020);
x = randn(n,k);
beta(1,1) = 1.0;
beta(2,1) = 1.0;
beta(3,1) = 1.0;

y = (IN-rho*W)\(x*beta) + (IN-rho*W)\(randn(n,1)*sqrt(sige)); 

info.lflag = 0; % don't use Pace-Barry lndet approximation
disp('maximum likelihood estimates');
result0 = sar(y,x,W,info);
prt(result0);

ndraw = 2500;
nomit = 500;
prior.novi = 1; % homoscedastic prior
prior.lflag = 0; % don't use Pace-Barry lndet approximation
prior.beta = zeros(k,1);    % prior mean of 0
prior.bcov = eye(k)*0.01;  % imposed very tightly

results = sar_g(y,x,W,ndraw,nomit,prior);
results.tflag = 'tstat';
disp('Bayesian tight prior estimates');
prt(results);

prior2.lflag = 0; % don't use Pace-Barry lndet approximation
prior2.novi = 1; % homoscedastic model 
prior2.beta = zeros(k,1);  % prior mean of 0
prior2.bcov = eye(k)*0.5;    % imposed with medium tightness

results2 = sar_g(y,x,W,ndraw,nomit,prior2);
results2.tflag = 'tstat';
disp('Bayesian medium prior estimates');
prt(results2);

b1mean = mean(results.bdraw)';
b2mean = mean(results2.bdraw)';

out = [beta result0.beta b1mean b2mean
       sige result0.sige results.sige results2.sige
       rho result0.rho  results.rho results2.rho
       0 result0.rsqr results.rsqr results2.rsqr];
   
in.cnames = strvcat('True Values','Max Lik','Bayes tight','Bayes medium');
in.rnames = strvcat('Parameters','beta0','beta1','beta2','sigma2','rho','r-squared');

fprintf(1,'\n comparison of estimates \n');
mprint(out,in);

% do a plot of posteriors for beta
for i=1:k;
[h1,f1,y1] = pltdens(results.bdraw(:,i));
[h2,f2,y2] = pltdens(results2.bdraw(:,i));

plot(y1,f1,'.r',y2,f2,'.g');
legend('posterior beta tight','posterior beta medium');
title(['posterior distribution beta =' num2str(i)]);
pause;
end;

% compute posterior probabilities for the two models
probs = model_probs(results,results2);

fprintf(1,'posterior probs for tight versus medium model \n');
in5.rnames = strvcat('Models','tight','medium');
mprint(probs,in5);


