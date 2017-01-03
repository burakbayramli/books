%This program fits a regression model with one
%unknown change point using generated data

clear;
clc;
rand('seed',sum(100*clock));
randn('seed',sum(100*clock));

%Set conditions for experiment and generate the data
nobs = 500;
lambda_true = 85;
sig_true = .2;
tau_true = .5;
theta1_true = 2;
theta2_true = 1;
beta1_true = 1.5;
beta2_true = .8;

x = randn(nobs,1);
bigX = [ones(nobs,1) x];

y = zeros(nobs,1);
for i = 1:nobs;
    if i<=lambda_true;
        y(i,1) = theta1_true + theta2_true*x(i,1) + sqrt(sig_true)*randn(1,1);
    elseif i > lambda_true
        y(i,1) = beta1_true + beta2_true*x(i,1) + sqrt(tau_true)*randn(1,1);
    end;
end;

%Set up the prior values;
mu_theta = zeros(2,1);
mu_beta = zeros(2,1);
V_theta = 100*eye(2);
V_beta = 100*eye(2);
a1=3;
a2=1/2;
b1=3;
b2 = 1/2;


%Begin the Gibbs sampler
iter =1000;
burn = 200;
lambda_grid = [1:1:nobs-1]';

betas_final = zeros(2,iter-burn);
thetas_final = betas_final;
sig_final = zeros(iter-burn,1);
tau_final = sig_final;
lambda_final = sig_final;

lambda = 250;
sig = 1;
tau = 1;

for i = 1:iter;
    %----------------------------------------------------
    %Sample the regression parameters theta and beta
    %----------------------------------------------------
    points_beta = [1:1:lambda];
    X_beta = bigX(points_beta,:);
    y_beta = y(points_beta);
    D_beta = inv(X_beta'*X_beta/sig + inv(V_beta));
    d_beta = X_beta'*y_beta/sig + inv(V_beta)*mu_beta;
    H_beta = chol(D_beta);
    betas = D_beta*d_beta + H_beta'*randn(2,1);
    
    points_theta = [lambda+1:1:nobs];
    X_theta = bigX(points_theta,:);
    y_theta = y(points_theta);
    D_theta = inv(X_theta'*X_theta/tau + inv(V_theta));
    d_theta = X_theta'*y_theta/tau + inv(V_theta)*mu_theta;
    H_theta = chol(D_theta);
    thetas = D_theta*d_theta + H_theta'*randn(2,1);
    
    %------------------------------------------
    %Sample the variance parameters sigma^2 and tau^2
    %----------------------------------------
    resids_beta = y_beta - X_beta*betas;
    sig = invgamrnd((length(resids_beta)/2) + a1, inv( inv(a2) + .5*resids_beta'*resids_beta),1,1);
    
    resids_theta = y_theta - X_theta*thetas;
    tau = invgamrnd((length(resids_theta)/2)+b1, inv(inv(b2) + .5*resids_theta'*resids_theta),1,1);
    
    %-----------------------------------
    %Sample the changepoint lambda
    %-----------------------------------
    
    dens_unnorm = zeros(nobs-1,1);
    for j = 1:nobs-1; %loop over the number of discrete values for Lambda
        y_keep1 = y(1:j);               y_keep2 = y(j+1:nobs);
        X_keep1 = bigX(1:j,:);          X_keep2 = bigX(j+1:nobs,:);
        
        first_part = normpdf(y_keep1,X_keep1*betas,sqrt(sig)*ones(j,1));
        second_part = normpdf(y_keep2,X_keep2*thetas,sqrt(tau)*ones(nobs-j,1));
        dens_unnorm(j,1) = prod(first_part)*prod(second_part);
    end;
    dens_norm = dens_unnorm/sum(dens_unnorm);
    lambda = discrete(lambda_grid,dens_norm);
    [lambda tau sig]
    
    if i > burn;
        betas_final(:,i-burn) = betas;
        thetas_final(:,i-burn) = thetas;
        sig_final(i-burn,1) = sig;
        tau_final(i-burn,1) = tau;
        lambda_final(i-burn,1) = lambda;
    end;
end;
disp('Post means and std deviatiations');
disp('For betas, thetas, sig, tau and lambda');
[mean(betas_final')' std(betas_final')']
[mean(thetas_final')' std(thetas_final')']
[mean(sig_final) std(sig_final)]
[mean(tau_final) std(tau_final)]
[mean(lambda_final) std(lambda_final)]