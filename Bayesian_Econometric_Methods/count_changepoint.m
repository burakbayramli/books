%This program fits a regression model with one
%unknown change point using generated data

clear;
clc;
rand('seed',sum(100*clock));
randn('seed',sum(100*clock));

load coaldata.txt;

y = coaldata(:,1);
nobs = length(y);

%Set conditions for experiment and generate the data
%gamma_true = 20;
%delta_true = 1
%nobs = 112;
%lambda = 20;

%y = zeros(nobs,1);
%for i = 1:nobs;
%    if i<=lambda
%    y(i,1) = poissrnd(gamma_true);
%    elseif i > lambda
%    y(i,1) = poissrnd(delta_true);
%    end;
%end;

%Set up the prior values;
%NOTE: In this exercise, we are using the routine "gam_rnd", 
%which is parameterized differently than the routine 
%in the book. In particular, the second argument 
%of the density is the reciprocal of how it is 
%parameterized in our book. Thus, we change the 
%hyperparameters below to accomodate for this 
%fact, and alter the conditional distributions 
%accordingly. 
a1 = 1; a2 = 1;
d1 = 1; d2 = 1;


%Begin the Gibbs sampler
iter =1000;
burn = 200;
lambda_grid = [1:1:nobs-1]';
lambda_date = [1851:1:1961]';

gamma_final = zeros(iter-burn,1);
delta_final = gamma_final;
lambda_final = gamma_final;

lambda = 50;


for i = 1:iter;
    %----------------------------------------------------
    %Sample gamma, the parameter of the "first" Poisson density
    %----------------------------------------------------
    y_gamma = y(1:lambda);
    n_lambda = length(y_gamma);
    gammas = gamm_rnd(1,1,(a1 + sum(y_gamma)),(a2 + n_lambda))
   
    %------------------------------------------
    %Sample delta, the parameter of the "second" Poisson density
    %----------------------------------------
    y_delta = y(lambda+1:nobs);
    deltas = gamm_rnd(1,1,(d1 + sum(y_delta)),(d2 + (nobs-n_lambda)));
    
    %-----------------------------------
    %Sample the changepoint lambda
    %-----------------------------------
    
    log_dens_unnorm = zeros(nobs-1,1);
    for j = 1:nobs-1; %loop over the number of discrete values for Lambda
       ypart1 = y(1:j);
       ypart2 = y(j+1:nobs);
       n_temp = length(ypart1);
       log_dens_unnorm(j,1) = sum(ypart1)*log(gammas) - n_temp*gammas + sum(ypart2)*log(deltas) - (nobs-n_temp)*deltas;
       %dens_unnorm(j,1) = (gammas^(sum(ypart1)))*exp(-n_temp*gammas)*(deltas^(sum(ypart2)))*exp(-(nobs-n_temp)*deltas);
    end;
    d = max(log_dens_unnorm);
    log_dens_unnorm = log_dens_unnorm -d;
    dens_unnorm = exp(log_dens_unnorm);
    dens_norm = dens_unnorm/sum(dens_unnorm);
    lambda = discrete(lambda_grid,dens_norm);
    lambda_keep = lambda_date(lambda);
    
    if i > burn;
        gamma_final(i-burn,1) = gammas;
        delta_final(i-burn,1) = deltas;
        lambda_final(i-burn,1) = lambda_keep;
    end;
end;
disp('Post means and std deviatiations');
disp('For betas, thetas, sig, tau and lambda');
[mean(gamma_final')' std(gamma_final')']
[mean(delta_final')' std(delta_final')']
[mean(lambda_final) std(lambda_final)]
