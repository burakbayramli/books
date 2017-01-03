%This m-file generates data and 
%fits the model using the uncentered parameterization and direct MC 

clear; clc;
randn('seed',sum(100*clock));

nobs = 10;
T=2;
sigeps = 1;
sigeta = 10;

mu = 2;

B = (sigeps*sigeta)/(T*sigeta + sigeps)

%----------------
%GENERATE THE DATA
%-----------------
y = [];
for i = 1:nobs;
    eta_i = mu + sqrt(sigeta)*randn(1,1);
    y_temp = ones(2,1)*eta_i + sqrt(sigeps)*randn(2,1);
    y = [y; y_temp];
end;


%DEFINE SOME TERMS NEEDED FOR MARGINAL POSTERIOR OF MU
Sigma_T = sigeps*eye(T) + sigeta*ones(T,T);
V_mu = (1/nobs)*inv(ones(1,T)*inv(Sigma_T)*ones(T,1));
final_part = 0;
for j = 1:nobs;
    y_keep = y(((2*j)-1):2*j);
    tempp = ones(1,T)*inv(Sigma_T)*y_keep;
    final_part = final_part + tempp;
end;
mu_hat = V_mu*final_part;


%RUN THE POSTERIOR SIMULATOR
iter = 1000;
for i = 1:iter;
    
%----------------------------
%MARGINAL POSTERIOR FOR MU
%-----------------------------
mu_draw = mu_hat + sqrt(V_mu)*randn(1,1);

%-----------------------------
%POSTERIOR CONDITIONAL FOR alpha's
%-----------------------------
ytilde = y - mu_draw;
for j = 1:nobs;
    y_keep = ytilde(((2*j)-1):2*j);
    DD = inv(T/sigeps + inv(sigeta));
    Dd = sum(y_keep)/sigeps;
    alpha_draws(j,1) = DD*Dd + sqrt(DD)*randn(1,1);
end;

alpha_represent1(i,1) = alpha_draws(3,1);
alpha_represent2(i,1) = alpha_draws(5,1);
alpha_represent3(i,1) = alpha_draws(9,1);
mu_final(i,1) = mu_draw;
end;

disp('Representative Correlations Between alpha_i and mu'); 
corrcoef(alpha_represent1,mu_final)
corrcoef(alpha_represent2,mu_final)
corrcoef(alpha_represent3,mu_final)