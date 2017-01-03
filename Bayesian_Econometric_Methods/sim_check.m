%This m-file runs the posterior simulator 
%check of Geweke (2004). 
clear;
clc;
randn('seed',sum(100*clock));

%------------------------
%MARGINAL-Conditonal Simulator
%------------------------
a = 3;
b = (1/2);
mu_beta = ones(2,1); 
V_beta = .25*eye(2);
H_beta = chol(V_beta);

M1 = 100000;
sigdraws = invgamrnd(a,b,M1,1);
betadraws = randn(M1,2);
for j = 1:M1;
betadraws(j,:) = (mu_beta + H_beta'*randn(2,1))';
end;

%--------------------------
%NOW, FIT THE MODEL USING THE Gibbs sampler (i.e., SUCCESSIVE CONDITIONAL SIMULATOR)
%-------------------------------
nobs = 10;
x = [ones(nobs,1) randn(nobs,1)];

M2 = M1;
sigdraws2 = zeros(M2,1);
betadraws2 = zeros(M2,2);
    
        %-------------------------------
        %Initial observation from prior
        %-------------------------------
        sigdraws2(1,1) = invgamrnd(a,b,1,1);
        betadraws2(1,:) = (mu_beta + H_beta'*randn(2,1))';
for j =2: M2;
    ynew = x*betadraws2(j-1,:)' + sqrt(sigdraws2(j-1,1))*randn(nobs,1);
    %
    D_b2 = inv(x'*x/sigdraws2(j-1,1) + inv(V_beta));
    %D_b2 = inv(x'*x/sigdraws2(j-1,1) + V_beta);
                                        %Remove comment above to analyze
                                        %results when V_beta is incorrectly
                                        %entered...
    d_b2 = x'*ynew/sigdraws2(j-1,1) + inv(V_beta)*mu_beta;
    %d_b2 = x'*ynew/sigdraws2(j-1,1) ;  
                                        %REMOVE COMMENT "%" ABOVE TO TRY OUT THE
    %                                   SPECIFICATION THAT IMPOSES A PRIOR
    %                                   MEAN = [0 0]'.
    
    H_b2 = chol(D_b2);
    betadraws2(j,:) = (D_b2*d_b2 + H_b2'*randn(2,1))';
    %
    resids_sum = sum( (ynew - x*betadraws2(j,:)')'*(ynew - x*betadraws2(j,:)') );
    sigdraws2(j,1) = invgamrnd( (nobs/2)+a, inv(inv(b) + .5*resids_sum),1,1);
    %sigdraws2(j,1) = invgamrnd( (nobs/2)+a, inv((b) + .5*resids_sum),1,1);
                                        %remove comment above to analyze
                                        %results when b instead of b^-1 is
                                        %used in the posterior conditional
                                        %for sigma^2
end;

%----------------------------
%ORDER MOMENT CONDITIONS AS FOLLOWS
%beta_const, beta_slope, sigma2, 
%-------------------------------
V_bc = (1/M1)*(sum(betadraws(:,1).^2)) - (mean(betadraws(:,1)))^2;
V_bs = (1/M1)*(sum(betadraws(:,2).^2)) - (mean(betadraws(:,2)))^2;
V_sig = (1/M1)*(sum(sigdraws.^2)) - (mean(sigdraws))^2;

V_bc2 = M2*num_variance(betadraws2(:,1));
V_bs2 = M2*num_variance(betadraws2(:,2));
V_sig2 = M2*num_variance(sigdraws2);

mean_differences = [mean(betadraws(:,1)) - mean(betadraws2(:,1)); ...
                    mean(betadraws(:,2)) - mean(betadraws2(:,2)); ...
                    mean(sigdraws) - mean(sigdraws2)];

            denom_part = [inv(M1)*V_bc + inv(M2)*V_bc2; ...
              inv(M1)*V_bs + inv(M2)*V_bs2; ...
              inv(M1)*V_sig + inv(M2)*V_sig2];
test_stats = mean_differences./(denom_part.^(1/2))      

        
        
       