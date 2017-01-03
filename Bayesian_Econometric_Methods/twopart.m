%generate data from the two part model, 
%and fit the model using the Gibbs sampler

clear;
clc;
randn('seed',sum(100*clock));

    %generate the data
nobs = 1000;
beta1_true = .5;
beta2_true = 1;
sigeps_true = 4;

eps1 = randn(nobs,1);
eps2 = randn(nobs,1);
Dstar_true = beta1_true + eps1;
D = .5 + .5*sign(Dstar_true);
y =zeros(nobs,1);
for i = 1:nobs;
    if D(i,1)==1
        logy = beta2_true + sqrt(sigeps_true)*eps2(i,1);
        y(i,1) = exp(logy);
    elseif D(i,1)==0;
        y(i,1) = 0;
    end;
end;
const = ones(nobs,1);
%prior values
mu_beta1 = 0;
mu_beta2 = 0;
V_beta1 = 10;
V_beta2 = 10;
a = 3;
b = 1;

%find the positive y observations, and assemble those data
points = find(D==1);
yplus = log(y(points));
nplus = length(yplus);
const_plus = ones(nplus,1);

        %Note: if there are covariates, you would need to 
        %get the positive part for the x data as well

iter = 3000;
burn = 1000;
beta1_final  = zeros(iter-burn,1);
beta2_final = beta1_final;
sigeps_final = beta1_final;
    beta1 = 0;
    beta2 = 0;
    sigeps = 1;

        for j = 1:iter;
            %draw from latent data dstar
            mu_vec = beta1*const;
            Dstar = truncnorm(mu_vec,const,D);
            
            %draw from beta1 conditional
            D_beta1 = inv(const'*const + inv(V_beta1));
            d_beta1 = const'*Dstar + inv(V_beta1)*mu_beta1;
            H_beta1 = chol(D_beta1);
            beta1 = D_beta1*d_beta1 + H_beta1'*randn(1,1);
            
            %draw from beta2 conditional
            D_beta2 = inv(const_plus'*const_plus/sigeps + inv(V_beta2));
            d_beta2 = const_plus'*yplus/sigeps + inv(V_beta2)*mu_beta2;
            H_beta2 = chol(D_beta2);
            beta2 = D_beta2*d_beta2 + H_beta2'*randn(1,1);
            
            %draw from sigeps conditional
            sigeps = invgamrnd( (nplus/2) + a, inv(inv(b) + .5*(yplus - beta2)'*(yplus - beta2)),1,1);
            
            if j > burn;
                beta1_final(j-burn,1) = beta1;
                beta2_final(j-burn,1) = beta2;
                sigeps_final(j-burn,1) = sigeps;
            end;
        end;
disp('Posterior means: beta1, beta2, sigeps');
[mean(beta1_final) mean(beta2_final) mean(sigeps_final)]
disp('True parameter values');
[beta1_true beta2_true sigeps_true]