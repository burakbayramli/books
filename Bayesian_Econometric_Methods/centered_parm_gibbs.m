%This m-file generates data and fits the model using 
%the Gibbs sampler and the centered parameterization

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




%RUN THE POSTERIOR SIMULATOR
iter = 1000;
burn = 100;
eta_draws = randn(nobs,1);

for i = 1:iter;
%----------------------------
%POSTERIOR Conditional FOR MU
%-----------------------------
D_eta = sigeta/nobs;
d_eta = sum(eta_draws)/sigeta;
mu_draw = D_eta*d_eta + sqrt(D_eta)*randn(1,1);

%-----------------------------
%POSTERIOR CONDITIONAL FOR eta's
%-----------------------------
for j = 1:nobs;
    y_keep = y((2*j-1):2*j);
    b = (1/sigeps)*sum(y_keep) + mu_draw/sigeta;
    eta_draws(j,1) = B*b + sqrt(B)*randn(1,1);
end;

if i > burn;
    eta_represent1(i-burn,1) = eta_draws(3,1);
    eta_represent2(i-burn,1) = eta_draws(5,1);
    eta_represent3(i-burn,1) = eta_draws(9,1);
    mu_final(i-burn,1) = mu_draw;
end;
end;
clc;

disp('Representative Correlations Between eta_i and mu'); 
corrcoef(eta_represent1,mu_final)
corrcoef(eta_represent2,mu_final)
corrcoef(eta_represent3,mu_final)

disp('Lag 1 Correlations - selected eta_i and mu');
L = length(eta_represent1);
a1 = eta_represent1(2:L);   aL1 = eta_represent1(1:L-1);
a2 = eta_represent2(2:L);   aL2 = eta_represent2(1:L-1);
a3 = eta_represent3(2:L);   aL3 = eta_represent3(1:L-1);
mu1 = mu_final(2:L);          muL1 = mu_final(1:L-1);

corrcoef(a1,aL1)
corrcoef(a2,aL2)
corrcoef(a3,aL3)
corrcoef(mu1,muL1)
