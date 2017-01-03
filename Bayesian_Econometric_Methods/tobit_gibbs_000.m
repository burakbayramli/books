%This m-file fits the gibbs sampler on the tobit data
clear; clc;

load tobit_data.txt;
weeks = tobit_data(:,1);
D = tobit_data(:,2);
D2 = tobit_data(:,3);
nobs = length(weeks);
const = ones(nobs,1);
y = weeks;
x = [const tobit_data(:,4:7)];
k = size(x,2);
nobs = length(x);
%set prior values 
mu_beta = zeros(k,1);
var_beta = 10^2*eye(k);
invarbeta = inv(var_beta);

a = 3;
b = 1/(2*20);
%initialize vectors and begin the Gibbs sampler
iter  = 5500;
burn = 500;
beta_final = zeros(iter-burn,k);
sig_final = zeros(iter-burn,1);
[bhat stderr tstat sig rsq] = ols(x,y);
beta = bhat;
sig = 500;
for i = 1:iter;
    %augmented latent data
    ztemp_aug1 = truncnorm(x*beta,sig,0);  %for those with y=0 (D=1), draw latent data to be truncated from above at zero
                                                                    %for now, do this for everyone.

    ztemp_aug2 = truncnorm2(x*beta,sig,52,999); %for those with y=52 (D2=1), draw latent data to be truncated from below at 52
                                                                            %for now, do this for everyone. 
                                                                            
    zaug = D.*ztemp_aug1 + D2.*ztemp_aug2 + (1-D- D2).*y; %Note that D,D2 are exclusive!
    
    %regression paramters
    D_beta = inv(x'*x/sig + invarbeta);
    d_beta = x'*zaug/sig + invarbeta*mu_beta;
    H = chol(D_beta);
    
    beta = D_beta*d_beta + H'*randn(k,1);
    
    %variance parameter
    
    sig = invgamrnd( (nobs/2) + a, inv( inv(b) + .5*sum( (zaug - x*beta).^2 ) ),1,1);
    
    if i >burn;
        beta_final(i-burn,:) = beta';
        sig_final(i-burn,:) = sig;
    end;
end;
clc;

disp('Means and Standard Deviations of parameters');
disp('const afqt spouse_inc kids ed, STD_DEV');
[mean(beta_final) mean(sqrt(sig_final))]
[std(beta_final) std(sqrt(sig_final))]

disp('Marginal Effects');
disp('const afqt spouse_inc kids ed, variance');
mu_x = mean(x);
probs = normcdf ( (52 - mu_x*beta_final')'./(sqrt(sig_final))) - normcdf((-mu_x*beta_final')'./(sqrt(sig_final)));
[mean(beta_final(:,2).*probs) mean(beta_final(:,3).*probs) mean(beta_final(:,4).*probs) mean(beta_final(:,5).*probs) ]
[std(beta_final(:,2).*probs) std(beta_final(:,3).*probs) std(beta_final(:,4).*probs) std(beta_final(:,5).*probs) ]
