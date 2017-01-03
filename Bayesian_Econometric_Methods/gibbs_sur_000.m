%This m-file runs a gibbs sampling routine 
%on a two-equation SUR model. 

clear;
clc;
randn('seed',sum(100*clock));
rand('seed',sum(100*clock));
%Set up design of experiment. 
nobs = 2500;
beta11 = 1; beta12 = .3;
beta21 = 2;  beta22 = -.7;
corr_true = -.45;
Sigma_true = [.5 corr_true*sqrt(.5)*sqrt(.3);
               corr_true*sqrt(.5)*sqrt(.3) .3];
H_true = chol(Sigma_true);

%Generate the data;
x1 = zeros(nobs,1);
x2 = x1;
y1 = x1;
y2 = x1;

for i = 1:nobs; 
    error_vec = H_true'*randn(2,1);
    x1(i,1) = randn(1,1);
    x2(i,1) = randn(1,1);
    y1(i,1) = beta11 + beta12*x1(i,1) + error_vec(1);
    y2(i,1) = beta21 + beta22*x2(i,1) + error_vec(2);
end;

%Define some terms that will be useful later when running the Gibbs
%sampler. 
bigy = [y1' y2']';
bigX1 = [ones(nobs,1) x1];
bigX2 = [ones(nobs,1) x2];
X1X1 = bigX1'*bigX1;    X1X2 = bigX1'*bigX2;
X2X1 = bigX2'*bigX1;    X2X2 = bigX2'*bigX2;
X1Y1 = bigX1'*y1;       X1Y2 = bigX1'*y2;
X2Y1 = bigX2'*y1;       X2Y2 = bigX2'*y2;
ytilde = zeros(2,nobs);
xtilde = zeros(2,4,nobs);
for i = 1:nobs; 
    ytilde(:,i) = [y1(i,1) y2(i,1)]';
    xtilde(:,:,i) = [1 x1(i,1) 0 0; 
                    0 0 1 x2(i,1)];
end;

%Define the prior hyperparameters;
mu_beta = zeros(4,1);
V_beta = 1000*eye(4);
Omega = eye(2);
nu = 4;

%Run the Gibbs sampler
iter = 1000;
burn = 200;
sig1_final = zeros(iter-burn,1);
sig2_final = sig1_final;
corr_final =sig1_final;
betas_final = zeros(4,iter-burn);

sig1 = 1; sig2 = 1; sig12 = 0;
for i = 1:iter;
    %Sample from Conditional for Beta
    matrix1 = [sig1*X1X1 sig12*X1X2;
               sig12*X2X1 sig2*X2X2];
    matrix2 = [ sig1*X1Y1 + sig12*X1Y2 ; 
                sig2*X2Y2 + sig12*X2Y1];
    D_beta = inv(matrix1 + inv(V_beta));        
    d_beta = matrix2 + inv(V_beta)*mu_beta;
    H_beta = chol(D_beta);
    betas = D_beta*d_beta + H_beta'*randn(4,1);
    
    %Sample from Wishart conditional for Sigmainv
    tempp = zeros(2,2);
    for j = 1:nobs;
        temp_term = (ytilde(:,j) - xtilde(:,:,j)*betas)*(ytilde(:,j) - xtilde(:,:,j)*betas)';
        tempp = temp_term + tempp;
    end;
    Sigmainv = wish_rnd(inv(tempp + inv(Omega)),nobs+nu);
    Sigma = inv(Sigmainv);
    sig1 = Sigmainv(1,1);
    sig2 = Sigmainv(2,2);
    sig12 = Sigmainv(1,2);
    
    s1 = Sigma(1,1);
    s2 = Sigma(2,2);
    s12 = Sigma(1,2);
    
    if i > burn;
        sig1_final(i-burn,1) = s1;
        sig2_final(i-burn,1) = s2;
        corr_final(i-burn,1) = s12/(sqrt(s1)*sqrt(s2));
        betas_final(:,i-burn,1) = betas;
    end;
end;

disp('Posterior means and true values - error variances and correlations');
[mean(sig1_final) .5;
 mean(sig2_final) .3;
 mean(corr_final) corr_true]

disp('Posterior means and true values - regression parameters');
[mean(betas_final')' [beta11 beta12 beta21 beta22]']

            

