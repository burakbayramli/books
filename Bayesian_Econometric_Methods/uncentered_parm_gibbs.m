%This m-file generates data and fits the model using 
%the Gibbs sampler and the uncentered parameterization


clear;clc;
randn('seed',sum(100*clock));

nobs = 10;
T=2;
sigeps = 1;
sigeta = 10;

mu = 2;

B = (sigeps*sigeta)/(T*sigeta + sigeps);
ntotal = nobs*T;
%----------------
%GENERATE THE DATA
%-----------------
y = [];
for i = 1:nobs;
    eta_i = mu + sqrt(sigeta)*randn(1,1);
    y_temp = ones(2,1)*eta_i + sqrt(sigeps)*randn(2,1);
    y = [y; y_temp];
end;

alpha_draws = randn(nobs,1);

%RUN THE POSTERIOR SIMULATOR
iter = 1000;
burn = 200;

for i = 1:iter;
    
%----------------------------
%POSTERIOR CONDITIONAL FOR MU
%-----------------------------
ystar = [];
for j = 1:nobs
    ystar_tempp = y( ((2*j)-1):2*j ) - alpha_draws(j,1)*ones(T,1);
    ystar = [ystar; ystar_tempp];
end;
D_mu = sigeps/ntotal;
d_mu = sum(ystar)/sigeps;
mu_draw = D_mu*d_mu + sqrt(D_mu)*randn(1,1);

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

if i > burn;
    alpha_represent1(i-burn,1) = alpha_draws(3,1);
    alpha_represent2(i-burn,1) = alpha_draws(5,1);
    alpha_represent3(i-burn,1) = alpha_draws(9,1);
    mu_final(i-burn,1) = mu_draw;
end;
end;
clc;

disp('Representative Correlations Between alpha_i and mu'); 
corrcoef(alpha_represent1,mu_final)
corrcoef(alpha_represent2,mu_final)
corrcoef(alpha_represent3,mu_final)

disp('Lag 1 Correlations - selected alpha_i and mu');
L = length(alpha_represent1);
a1 = alpha_represent1(2:L);   aL1 = alpha_represent1(1:L-1);
a2 = alpha_represent2(2:L);   aL2 = alpha_represent2(1:L-1);
a3 = alpha_represent3(2:L);   aL3 = alpha_represent3(1:L-1);
mu1 = mu_final(2:L);          muL1 = mu_final(1:L-1);

corrcoef(a1,aL1)
corrcoef(a2,aL2)
corrcoef(a3,aL3)
corrcoef(mu1,muL1)
