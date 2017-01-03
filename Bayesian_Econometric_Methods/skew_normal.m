%this m-file tries to apply the skew link idea 
%to a standard regression problem
%y = beta0  +beta1x + deltaz + epsilon,
%where z is generated from a half-normal distribution
clear;
clc;
randn('seed',sum(100*clock));


load wages.raw;
y = wages(:,1);
x = [ones(length(y),1)];

[nobs,k] = size(x)




%*************************
%FIT THE SKEWED REGRESSION
%*************************

%set prior values
mu_beta = zeros(k,1);
V_beta = 10*eye(k);
mu_delta = 0;
V_delta = 10;
mu_parms = [mu_beta' mu_delta]';
V_parms = blockdiag(V_beta,V_delta);
invV_parms = inv(V_parms);

a = 3;
b = (1/(2*1));

iter = 5000;
burn = 2000;
betas_final = zeros(iter-burn,k);
delta_final = zeros(iter-burn,1);
sig_final = zeros(iter-burn,1);

betas = zeros(k,1);
delta = 0;
sig=1;

%x = (xtilde-mean(xtilde))/std(xtilde);
    %-----------------------
    %START THE GIBBS SAMPLER
    %-----------------------
for j = 2:iter;

%sample z, the skewed data
mu_truncz = (delta/(sig + delta^2))*(y - x*betas);
var_truncz = sig/(sig + delta^2);
zuse = truncnorm2(mu_truncz,var_truncz,0,999);


%sample the regression and skew parameter jointly
bigx_mat = [x zuse];
D_parms = inv(bigx_mat'*bigx_mat/sig + invV_parms);
d_parms = bigx_mat'*y/sig;   %assumed prior mean of zero
H = chol(D_parms);
parms = D_parms*d_parms + H'*randn(k+1,1);
betas = parms(1:k,1)
delta = parms(k+1,1)

%sample the error variance parameter
resids = y - bigx_mat*parms;
sig = invgamrnd( (nobs/2) +a, inv( inv(b) + .5*resids'*resids),1,1);


    if j > burn;
        betas_final(j-burn,:) = betas';
        delta_final(j-burn,1) = delta';
        sig_final(j-burn,1) = sig;    
    end;
end;
clc;

beta_hat = mean(betas_final)
delta_hat = mean(delta_final)
%----------------------------------
%PLOT NONPARAMETRIC AND SKEW-NORMAL DENSITY
%----------------------------------
clf; 
delta =delta_hat;
sig = mean(sig_final);
mu = beta_hat;
varian = sig + delta^2;

ygrid = linspace(0,40,5000);


densa = 2*(1/sqrt(2*pi*varian))*exp(- (1/(2*varian))*( (ygrid-mu).^2));
densb = normcdf( (delta/(sqrt(sig)*sqrt(varian)))*(ygrid-mu) );
density = densa.*densb;
plot(ygrid,density);
hold on; 

[dom ran] = epanech2(y);
plot(dom,ran,':r');
axis([0 50 0 max([max(density) max(ran)]) ]);
hold off;


