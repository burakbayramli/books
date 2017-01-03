%This m-file estimates an ordered probit 
%model using generated data, and the 
%algorithm of Albert and Chib (1993). 
clear;
clc;
randn('seed',sum(100*clock));
rand('seed',sum(100*clock));

%Set the parameters of the generated data
nobs = 500;
L = 3;
beta_0 = .5; 
beta_1 = .3;
x = randn(nobs,1);
z = beta_0 + beta_1*x + randn(nobs,1);
big_x = [ones(nobs,1) x];

alpha_2 = 1; 
%create the observed y variable based on the 
%latent data
y = zeros(nobs,1);
D1 = y;
D2 = y;
D3 = y;
for i = 1:nobs;
    if z(i,1)<0
        y(i,1) = 1;
        D1(i,1) = 1;
    elseif z(i,1) > 0 & z(i,1) < alpha_2
        y(i,1)=2;
        D2(i,1)=1;
    else
        y(i,1) = 3;
        D3(i,1)=1;
    end;
end;

points_2 = find(D2==1);
points_3 = find(D3==1);

%intial conditions
alph = .5;
betas  = [0 0]';
iter = 2500;
burn = 500;
betas_final = zeros(iter-burn,size(big_x,2));
alph_final = zeros(iter-burn,1);

for i = 1:iter
    
     %posterior conditional for z
    zaug = D1.*truncnorm(big_x*betas,1,0) + D2.*truncnorm2(big_x*betas,1,0,alph) + D3.*truncnorm2(big_x*betas,1,alph,999);
    
    %posterior conditional for beta
    D_beta = inv( big_x'*big_x );
    d_beta = big_x'*zaug;
    H_beta = chol(D_beta);
    betas = D_beta*d_beta + H_beta'*randn(size(big_x,2),1);
    
  
   
    %posterior conditional for alpha
    m1 = max([0 max(zaug(points_2))]);
    m2 = min(zaug(points_3));
    u = rand(1,1);
    alph = m1 + (m2-m1)*u;
    if i > burn
        betas_final(i-burn,:) = betas';
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
    lagcorr(j,1) = tempp(1,2);
end;
xgrid = linspace(1,20,20)';
bar(xgrid,lagcorr,'y');

save ordprob_dat x z big_x D1 D2 D3 y xgrid lagcorr;
