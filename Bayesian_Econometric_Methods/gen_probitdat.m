%This program illustrates the gibbs sampler on a 
%probit model
clear; 
clc;

randn('seed',sum(100*clock));

sigma = 1;      %this is the standard deviation parm
beta0 = .2;
beta1 = .6;
nobs = 2000;
x_use = randn(nobs,1);
resids = randn(nobs,1);
cons = ones(nobs,1);

z = beta0 + beta1*x_use + sigma*resids;
D = .5*sign(z) + .5;

%get starting values and data? 
x = [cons x_use];


%Specify prior values

mubeta = zeros(size(x,2),1);
varbeta = 10^2*eye(size(x,2));
invarbeta = inv(varbeta);
a = 3; 
b = (1/(2*(1)));
    %sets prior mean and standard deviation 
    %of variance parameter* equal to 1. 


iter = 2500;
beta = zeros(iter,size(x,2));
sig = zeros(iter,1);    %variance parameter.
beta(1,:) = [beta0 beta1];
sig(1) = sigma;
%begin the Gibbs Sampler
burn = 500;
sig_final = zeros(iter-burn,1);
beta_final = zeros(iter-burn,size(x,2));
for i = 2:iter;i
   %For the latent data
   zaug = truncnorm(x*beta(i-1,:)',(sig(i-1,1)*ones(nobs,1)),D);
   
   %do beta part
   D_beta = inv(x'*x/sig(i-1,1) + invarbeta);
   d_beta = x'*zaug/sig(i-1,1) + invarbeta*mubeta;
   H = chol(D_beta);
   beta(i,:) = ( D_beta*d_beta + H'*randn(size(x,2),1) )';
  
   %do inverse gamma part
   sig(i,1) = invgamrnd( (nobs/2 + a), inv( inv(b) + .5*sum( (zaug - x*beta(i,:)').^2 ) ),1,1);
   if i > burn;
       beta_final(i-burn,:) = beta(i,:);
       sig_final(i-burn,:) = sig(i,:);
   end;
end;
parm_ratio1 = beta_final(:,2)./(sqrt(sig_final));
parm_ratio2 = beta_final(:,1)./sqrt(sig_final);
clc;
subplot(221),
plot(beta_final(:,2));
ylabel('\beta_1');
subplot(222),
plot(sqrt(sig_final));
ylabel('\sigma');
subplot(223),
plot(parm_ratio2);
ylabel('\beta_0 / \sigma');
subplot(224),
plot(parm_ratio1);
ylabel('\beta_1 / \sigma');
