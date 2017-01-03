%try out different methods for calculating the marginal 
%likelihood with a Norma-Gamma prior. 
clear;
clc;
randn('seed',sum(100*clock));

%generate the data
nobs = 100;
beta0 = 1;
beta1 = .5;
sigsq = .2;
x = randn(nobs,1);
y = beta0 + beta1*x + sqrt(sigsq)*randn(nobs,1);
big_x = [ones(nobs,1) x];

%select the prior hyperparameters;
a = 3;
b = 1/(2*.2);
mu_beta = zeros(2,1);
V_beta = 4*eye(2);
invV_beta = inv(V_beta);
k = size(big_x,2);

%------------------------------------------------
%calculate the marginal likleihood analytically;
%------------------------------------------------
det_term = (det(V_beta))*det( (invV_beta + big_x'*big_x));
inv_term = eye(nobs) - big_x*(inv(invV_beta + big_x'*big_x))*big_x';
normcons = ( (2*pi)^(-nobs/2))*(det_term^(-1/2))*Gamma( (nobs/2)+a)*(b^(-a))*inv(Gamma(a));
kernel = (inv(b) + .5*y'*inv_term*y)^(-( (nobs/2) + a));
marglike_true = normcons*kernel;
disp('Analytical Calculation of Log Marginal Likelihood');
log(marglike_true)

%-------------------------------------------------
%calculate the log ML using Chib's method
%-------------------------------------------------
%first, run the Gibbs sampler

iter = 20000;
burn = 100;
sig = 1;
betas_final = zeros(iter-burn,k);
sig_final = zeros(iter-burn,1);

for i = 1:iter;
       %sample beta
       D_beta = sig*inv(big_x'*big_x + invV_beta);
       d_beta = big_x'*y/sig + invV_beta*mu_beta/sig;
       H = chol(D_beta);
       betas = D_beta*d_beta + H'*randn(k,1);
       
       %sample sig
       resids  =y - big_x*betas;
       sig = invgamrnd( (nobs/2) + (k/2) +a, inv( inv(b) + .5*resids'*resids + .5*betas'*invV_beta*betas),1,1);
       
       if i > burn;
       betas_final(i-burn,:) = betas';
       sig_final(i-burn,1) = sig;
   end;
end;

%compute posterior means for evaluating liklihood, prior and posterior
%terms:
bpoint = mean(betas_final);
sigpoint = mean(sig_final);

%evaluate likelihood and prior at posterior mean values. 
like_part = ((2*pi)^(-nobs/2))*(sigpoint^(-nobs/2))*exp(-(1/(2*sigpoint))*(y-big_x*bpoint')'*(y-big_x*bpoint'));
prior_beta = ((2*pi)^(-k/2))*(sigpoint^(-k/2))*((det(V_beta))^(-1/2))*exp(-(1/(2*sigpoint))*bpoint*invV_beta*bpoint');
prior_sig = (inv(Gamma(a)))*(b^(-a))*(sigpoint^(-(a+1)))*exp(-1/(b*sigpoint));

%now, calculate the marginal
%posterior ordinate for beta using Rao-Blackwellization:
dens_beta_cond = zeros(length(sig_final),1);
mu_beta_cond = inv(big_x'*big_x + invV_beta)*(big_x'*y + invV_beta*mu_beta);
for i = 1:length(sig_final);   
    var_beta_cond = sig_final(i,1)*inv(big_x'*big_x + invV_beta);
    dens_beta_cond(i,1) = ((2*pi)^(-k/2))*((det(var_beta_cond))^(-1/2))*exp(-(1/2)*(bpoint'-mu_beta_cond)'*inv(var_beta_cond)*(bpoint'-mu_beta_cond));
end;
dens_beta = mean(dens_beta_cond);

%get the sigma^2 | beta conditional
new_a = (nobs/2) + (k/2) + a;
new_b = inv( inv(b) + .5*(y - big_x*bpoint')'*(y - big_x*bpoint') + .5*bpoint*invV_beta*bpoint');
dens_sig = (inv(Gamma(new_a)))*(new_b^(-new_a))*(sigpoint^(-(new_a+1)))*(exp(-1/(new_b*sigpoint)));

chib_ml =  (like_part*prior_beta*prior_sig)/(dens_beta*dens_sig);
disp('Log Marginal Likelihood Via Chib (1995)');
log(chib_ml)

%---------------------------------
%calculate the ML using Gelfand-Dey method
%---------------------------------
mean_vec = [bpoint sigpoint]';
add_on = zeros(k+1,k+1);
for i = 1:length(sig_final);
    temp_term = ([betas_final(i,:) sig_final(i,1)]' - mean_vec)*([betas_final(i,:) sig_final(i,1)]' - mean_vec)';
    add_on = add_on + temp_term;
end;
cov_mat = (1/length(sig_final))*add_on;
det_cov = det(cov_mat);
inv_cov = inv(cov_mat);
np = k+1;
%loop and calculate the "weight" for each post-convergence draw;
weight_GD = zeros(length(sig_final),1);
for i = 1:length(sig_final);
    parms = [betas_final(i,:) sig_final(i,1)]';
    critical = (parms - mean_vec)'*inv_cov*(parms-mean_vec);
    if critical < 11.34; %99th percentile of chisquare (3) density. 
    prior_b = ((2*pi)^(-k/2))*(sig_final(i,1)^(-k/2))*((det(V_beta))^(-1/2))*exp(-(1/(2*sig_final(i,1)))*betas_final(i,:)*invV_beta*betas_final(i,:)');
    prior_s = (inv(Gamma(a)))*(b^(-a))*(sig_final(i,1)^(-(a+1)))*exp(-1/(b*sig_final(i,1)));
    likefun = ((2*pi)^(-nobs/2))*(sig_final(i,1)^(-nobs/2))*exp(-(1/(2*sig_final(i,1)))*(y-big_x*betas_final(i,:)')'*(y-big_x*betas_final(i,:)'));
    f_part = (1/.99)*((2*pi)^(-np/2))*(det_cov^(-1/2))*exp(-.5*(parms - mean_vec)'*inv_cov*(parms-mean_vec));
    weight_GD(i,1) = f_part/(prior_b*prior_s*likefun);
end;
end;
weights_use = nonzeros(weight_GD); %discard those draws falling outside the region of truncation
disp('Log Marginal Likelihood via Gelfand-Dey');
GD_marglike = log(inv(mean(weights_use)))