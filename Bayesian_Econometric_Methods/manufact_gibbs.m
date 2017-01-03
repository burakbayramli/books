%This m-file runs a Gibbs sampler on the 
%manufacturing data, without imposing any 
%inequality constraints. 

clear;
clc;
randn('seed',sum(100*clock));

load prod_function;
labor = prod_function(:,1);
capital = prod_function(:,2);
output = prod_function(:,3);

y = log(output);
L = log(labor);
K = log(capital);

nobs = length(y)
X = [ones(nobs,1) L K];

%-----------
%Priors
%-----------
V_beta = 10*eye(3);

iter = 3000;
burn = 1000;
rts_final = zeros(iter-burn,1);

sigeps = 1;
for i = 1:iter;
    %--------------
    %Draw Betas
    %--------------
    D_beta = inv(X'*X/sigeps + inv(V_beta));
    d_beta = X'*y/sigeps;
    H_beta = chol(D_beta);
    beta_draws = D_beta*d_beta + H_beta'*randn(3,1);
    
    %-----------
    %Draw sigma^2
    %------------
    resid = y - X*beta_draws;
    sigeps = invgamrnd( (nobs/2), inv(.5*resid'*resid),1,1);
    
    if i > burn;
        rts_final(i-burn,1) = beta_draws(2) + beta_draws(3);
    end;
end;

save man_gibbs rts_final;
[dom ran] = epanech2(rts_final);
plot(dom,ran);
xlabel('Return to Scale');
ylabel('Density');
