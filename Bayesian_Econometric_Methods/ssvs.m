%This m-file runs the SSVS algorithm using a 
%generated data set
%
%generate data from y = beta0 + beta1x1 + beta2x2 + beta3x3 + u, 
%but allow for 5 potential explanatory variables. 
%set conditions for design of experiment
clear;
clc;
randn('seed',sum(100*clock));
rand('seed',sum(100*clock));

nobs = 1000;
covmat = [ 1 .4 .4 0 .6;
          .4  1 .7 0 .3;
           .4 .7 1 0 .3;
           0 0 0 1 0 ;
           .6 .3 .3 0 1];
H = chol(covmat);

beta0 = 2;
beta1 = .25;
beta2 = -.4;
beta3 = .6;

sig = .2^2;

x_mat = zeros(nobs,5);
for i = 1:nobs;
    xmat(i,:) = (H'*randn(5,1))';
    y(i,1) = beta0 + beta1*xmat(i,1) + beta2*xmat(i,2) + beta3*xmat(i,3) + sqrt(sig)*randn(1,1);
end;
xmat = [ones(nobs,1) xmat];

%set some initial conditions and prior hyperparameters
V0= 10^2;
tau2 = .0000001;
c2 = (9)/tau2;
mu_sigeps = .15;
a = 3;
b = (1/(2*mu_sigeps));

gammas = ones(5,1);
sigsq = 1;


iter = 10000;
burn = 200;
gammas_final = zeros(iter-burn,5);
betas_final = zeros(iter-burn,6);
sig_final = zeros(iter-burn,1);

for i = 1:iter;
    
    %-----------------
    %sample the betas
    %-----------------
    V_betas_temp = [V0 ;
        (gammas(1)*c2*tau2 + (1-gammas(1))*tau2);
        (gammas(2)*c2*tau2 + (1-gammas(2))*tau2);
        (gammas(3)*c2*tau2 + (1-gammas(3))*tau2);
        (gammas(4)*c2*tau2 + (1-gammas(4))*tau2);
        (gammas(5)*c2*tau2 + (1-gammas(5))*tau2)];
    V_betas = diag(V_betas_temp);
    D_beta = inv(xmat'*xmat/sigsq + inv(V_betas));
    d_beta = xmat'*y/sigsq ;
    H_beta = chol(D_beta);
    betas = D_beta*d_beta + H_beta'*randn(6,1);
    
    %-----------------
    %sample the gammas
    %-----------------
    for j = 1:5;
        numerator = normpdf(betas(j+1),0,sqrt(c2*tau2));
        denominator  = numerator + normpdf(betas(j+1),0,sqrt(tau2));
        prob = numerator/ denominator;
        unif = rand(1,1);
        gammas(j,1) = .5*sign(prob-unif) + .5;
    end;
    
    %---------------
    %sample the variance parameter
    %---------------
    resid = (y-xmat*betas)'*(y - xmat*betas);
    sigsq = invgamrnd((nobs/2) + a, inv( inv(b) + .5*resid),1,1);
    
    if i > burn;
        betas_final(i-burn,:) = betas';
        sig_final(i-burn,1) = sigsq;
        gammas_final(i-burn,:) = gammas';
    end;
end;

mean(betas_final)
mean(sig_final)
mean(gammas_final)
    