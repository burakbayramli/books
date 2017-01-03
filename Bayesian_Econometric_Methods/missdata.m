%this m-file generates data and fits 
%the missing data model, using only 
%the complete observations, and also 
%including the missing x data in the MCMC
%algorithm
clear;
clc;
randn('seed',sum(100*clock));

n = 2000;       %total # of observations    
nmiss = 1500;   %number of missing x's
nobs = n-nmiss; %number of observed x's
                %note: we will take the first 500 nobs as observed

beta0 = 2;
beta1 = -.7;
theta0 = 1;
theta1 = .35;
sigu = .4;
sigeps = .01;
z = randn(n,1);
Z = [ones(n,1) z];
x = theta0 + theta1*z + sqrt(sigeps)*randn(n,1);
y = beta0 + beta1*x + sqrt(sigu)*randn(n,1);

mu_beta = zeros(2,1);
mu_theta = mu_beta;
V_beta = 10*eye(2);
V_theta = V_beta;
a1 = 3;
a2 = 3;
b1 = 1/(2*.25);
b2 = b1;
%--------------------------------------------------------------------------
%APPROACH #1
%-------------------------------------------------------------------------

%First, fit the model using only the first 500 observations
%(which actually correspond to the observed x data)
%we will add a "_a" to the parameters to denote they come from 
%this first approach, which uses only the first 500 obs. 
iter = 5000;
burn = 1000;
y_observed = y(1:nobs);
x_observed = x(1:nobs);
X_observed = [ones(nobs,1) x_observed];
betas_a_final = zeros(iter-burn,2);
sigu_a_final = zeros(iter-burn,1);
sigu_a = 1;
for i = 1:iter;
    %sample the betas
    D_b = inv(X_observed'*X_observed/sigu_a + inv(V_beta));
    d_b = X_observed'*y_observed/sigu_a + inv(V_beta)*mu_beta;
    H_b = chol(D_b);
    betas_a = D_b*d_b + H_b'*randn(2,1);
    
    %sample sigma^2
    resids_a = y_observed - X_observed*betas_a;
    sigu_a = invgamrnd((nobs/2) + a1, inv(inv(b1) + .5*resids_a'*resids_a),1,1);
    
    if i > burn;
        betas_a_final(i-burn,:) = betas_a';
        sigu_a_final(i-burn,1) = sigu_a;
    end;
end;
    
[dom1a ran1a]  =epanech2(betas_a_final(:,1));
[dom2a ran2a] = epanech2(betas_a_final(:,2));

%--------------------------------------------------------------------------
%APPROACH #2
%-------------------------------------------------------------------------

%now, fit the full model, using all 2000 observations, upon including the 
%missing x data in the sampler. (parameters in this second run 
%have a "b" index, such as betas_b, while those in the above 
%run will have an "_a" index. 

iter = 5000;
burn = 1000;
betas_b_final = zeros(iter-burn,2);
thetas_b_final = betas_b_final;
sigu_b_final = zeros(iter-burn,1);
sige_b_final = zeros(iter-burn,1);

thetas_b = [0 0]';
betas_b = [0 0]';
sigu_b = 1;
sige_b = 1;

zmissing = z(nobs+1:n);
Zmissing = [ones(n-nobs,1), zmissing];
ymissing = y(nobs+1:n);     %select of z and y for missing x's.
for i = 1:iter;
    %sample missing latent data, xstar
    index = Zmissing*thetas_b;
    covterm = (betas_b(2)^2)*sige_b;
    varterm = sigu_b + covterm;
    meanpart = index + (betas_b(2)*sige_b/varterm)*(ymissing - betas_b(1) - betas_b(2)*index);
    varpart = sige_b*(1 - covterm^2/(sige_b*varterm));
    xtemp = meanpart + sqrt(varpart)*randn(n-nobs,1);
    xstar = [x(1:nobs)' xtemp']'; %replace last 1500 observations with xtemp
    Xstar = [ones(n,1) xstar];
    
    %draw theta parmaters
    
    D_theta = inv(Z'*Z/sige_b + inv(V_theta));
    d_theta = Z'*xstar/sige_b + inv(V_theta)*mu_theta;
    H_theta = chol(D_theta);
    thetas_b = D_theta*d_theta + H_theta'*randn(2,1);
    
    
    %draw sige
    resids_e = xstar - Z*thetas_b;
    sige_b = invgamrnd(n/2 + a2, inv(inv(b2) + .5*resids_e'*resids_e),1,1);
    
    %draw beta parameters
    D_beta = inv(Xstar'*Xstar/sigu_b + inv(V_beta));
    d_beta = Xstar'*y/sigu_b + inv(V_beta)*mu_beta;
    H_beta = chol(D_beta);
    betas_b = D_beta*d_beta + H_beta'*randn(2,1);
    
    %draw sigu
    resids_u = y - Xstar*betas_b;
    sigu_b = invgamrnd(n/2 + a1, inv(inv(b1) + .5*resids_u'*resids_u),1,1);
    
   if i > burn;
        betas_b_final(i-burn,:) = betas_b';
        thetas_b_final(i-burn,:) = thetas_b';
        sigu_b_final(i-burn,1) = sigu_b;
        sige_b_final(i-burn,1) = sige_b;    
    end;
end;
disp('Beta post. means: observed only first, full model second')
mean(betas_a_final)
mean(betas_b_final)
disp('sigma^2_u post means: observed only first, full model second')
mean(sigu_a_final)
mean(sigu_b_final)
disp('Post means of theta and sigma^2_e, respectively (full model)')
mean(thetas_b_final)
mean(sige_b_final)
disp('---------------------');    
disp('Post Std. of betas: observed only first, full model second');
betas_use = takemth(betas_b_final,10); %take every 10th draw to make sure 
                                        %autocorrelation is not artifically
                                        %making the std. errors look
                                        %smaller
                                        
std(betas_a_final)
std(betas_use)