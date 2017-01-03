%This m-file estimates an ordered probit 
%model using generated data, and the 
%algorithm of Albert and Chib (1993). 
clear;
clc;
randn('seed',sum(100*clock));
rand('seed',sum(100*clock));

load ordprob_dat;
%Set the parameters of the generated data
%nobs = 750;
%L = 3;
%beta_0 = 0; 
%beta_1 = .3;
%x = randn(nobs,1);
%z = beta_0 + beta_1*x + randn(nobs,1);
%big_x = [ones(nobs,1) x];

%alpha_2 = .5; 
%create the observed y variable based on the 
%latent data
%y = zeros(nobs,1);
%D1 = y;
%D2 = y;
%D3 = y;
%for i = 1:nobs;
%    if z(i,1)<0
%        y(i,1) = 1;
%        D1(i,1) = 1;
%    elseif z(i,1) > 0 & z(i,1) < alpha_2
%        y(i,1)=2;
%        D2(i,1)=1;
%    else
%        y(i,1) = 3;
%        D3(i,1)=1;
%    end;
%end;
points_2 = find(D2==1);
points_3 = find(D3==1);
nobs = length(y);

%intial conditions
delt = 1;
betas  = [0 0]';
iter = 2500;
burn = 500;
betas_final = zeros(iter-burn,size(big_x,2));
alph_final = zeros(iter-burn,1);

for i = 1:iter
    
    %posterior conditional for z
    zaug = D1.*truncnorm(big_x*betas,delt,0) + D2.*truncnorm2(big_x*betas,delt,0,1) + D3.*truncnorm2(big_x*betas,delt,1,999);
    
    %posterior conditional for beta
    D_beta = inv( big_x'*big_x/ (delt));
    d_beta = big_x'*zaug/delt;
    H_beta = chol(D_beta);
    betas = D_beta*d_beta + H_beta'*randn(size(big_x,2),1);
    
    %posterior conditional for delta
    delt = invgamrnd( ((nobs + size(big_x,2))/2), inv( .5*(zaug - big_x*betas)'*(zaug - big_x*betas)),1,1);
%delt = (1/alpha_2)
    alph = 1/sqrt(delt);
    betas_use = betas*(alph);
    if i > burn
        betas_final(i-burn,:) = betas_use';
        alph_final(i-burn,1) = alph;
    end;
end;

disp('Betas results')
mean(betas_final)
std(betas_final)

disp('Cutpoint results')
mean(alph_final)
std(alph_final)

%Now, calculate the lagged autocorrelations up to lag order 20
m = length(alph_final);
for j = 1:20; 
    alpha1 = alph_final(1:m-j,1);
    alpha2 = alph_final(j+1:m,1);
    tempp = corrcoef(alpha1,alpha2);
    lagcorr2(j,1) = tempp(1,2);
end;
xgrid = linspace(1,20,20)';
bar(xgrid,lagcorr2,'y');
xlabel('Lag');
ylabel('Correlation');
