%This function runs the Gibbs sampler 
%using the Rat growth data. 
clear; clc;
randn('seed',sum(100*clock));
load rats.raw.txt; %MAY NEED TO CHANGE if the .txt extension is not on your saved data set !!!
[nn,kk] = size(rats);
xuse = [ones(5,1) [8 15 22 29 36]'];

%-----------------------------
%Declare the prior values
%-----------------------------
a = 3;
b = 1/(2*20);
    %this chooses the prior mean for 
    %sigma^2 equal to 20 with std
    %also equal to 20
    
eta = [100 15]'
C = [40^2 0; 0 10^2];
    %this chooses the prior to center
    %the weight at birth at 100, and 
    %the growth rate at 15. C makes 
    %these choices reasonably diffuse

rho =5;
R = [10^2 0; 0 .5^2];

    %this prior specifies some degree 
    %of variation across rats, and does 
    %not restrict them to have equal 
    %birth weights and growth rates. 
    
   % for i = 1:1000;
   %     tempp = wish_rnd(inv(rho*R),rho);
   %     tempp2 = inv(tempp);
   %     std1(i,1) = sqrt(tempp2(1,1));
   %     std2(i,1) = sqrt(tempp2(2,2));
   % end;
  %  hist(std1,50);
  %  hist(std2,50);
    
iter = 10000;
    %declare the number of iterations
burn = 500;
    %declare the length of the burn-in

%---------------------------------
%Set lengths of parmaeter vectors
%---------------------------------
sigma2 = zeros(iter,1);
theta0 = zeros(2,iter);
theta_int = zeros(30,iter);
theta_rate = zeros(30,iter);
invSigma = zeros(2,2,iter);
var_int = zeros(iter,1);
var_rate = zeros(iter,1);
correl = zeros(iter,1);

%----------------------
%Set initial conditions
%----------------------
sigma2(1) = 20;
invSigma(:,:,1) = inv([100 0; 0 1]);
theta0(:,1) = [100 10]';

%Begin the Gibbs Sampler
for i = 2:iter;
    total_resid = 0;
    %Do conditional for all theta_i
    for j=1:30;
        Dtemp = inv(xuse'*xuse/sigma2(i-1) + invSigma(:,:,i-1) );
        dtemp = xuse'*rats(j,2:kk)'/sigma2(i-1) + invSigma(:,:,i-1)*theta0(:,i-1);
        H = chol(Dtemp);
        theta_temp = Dtemp*dtemp + H'*randn(2,1);
        theta_int(j,i) = theta_temp(1,1);
        theta_rate(j,i) = theta_temp(2,1);    
    
        %use this later for sigma^2 conditional
        resids =rats(j,2:kk)' - xuse*[theta_int(j,i) theta_rate(j,i)]';
        tempp = sum(resids.^2);
        total_resid = total_resid+tempp;
    end;
    thetabar = [mean(theta_int(:,i)) mean(theta_rate(:,i))]';
    
    %Do conditional for theta0
    Dtemp = inv(30*invSigma(:,:,i-1) + inv(C));
    dtemp = 30*invSigma(:,:,i-1)*thetabar + inv(C)*eta;
    H = chol(Dtemp);
    theta0(:,i) = Dtemp*dtemp + H'*randn(2,1);
    
    %Do Conditional for sigma2
    
    sigma2(i,1) = invgamrnd((150/2)+a, inv( .5*total_resid + inv(b)),1,1);
    
    %Do Conditional for Sigma^-1. 
    tempp3 = 0;
    for jj = 1:30;
        tempp1 = [theta_int(jj,i) theta_rate(jj,i)]' - theta0(:,i);
        tempp2 = tempp1*tempp1';
        tempp3 = tempp3 + tempp2;
    end;
    invSigma(:,:,i) = wish_rnd(inv(tempp3 + rho*R), 30+rho);
    
    Sigma = inv(invSigma(:,:,i));
    var_int(i,1) = Sigma(1,1);
    var_rate(i,1) = Sigma(2,2);
    correl(i,1)  = Sigma(1,2)/(sqrt(Sigma(1,1))*sqrt(Sigma(2,2)));
    
end;

int_use = theta_int(:,burn+1:iter);
rate_use = theta_rate(:,burn+1:iter);

mu_ints = mean(int_use')';
mu_rates = mean(rate_use')';


save ratgibbs mu_ints mu_rates;

clc;
disp('Posterior means of parameters');
disp('Order: common intercept, common rate, variance_intercept, variance_rate, correlation');
disp('intercept(10), intercept(25), rate(10), rate(25)');
parms = [theta0(1,burn+1:iter)' theta0(2,burn+1:iter)' var_int(burn+1:iter) var_rate(burn+1:iter) correl(burn+1:iter)...
        theta_int(10,burn+1:iter)' theta_int(25,burn+1:iter)' theta_rate(10,burn+1:iter)' theta_rate(25,burn+1:iter)'];
mean(parms)
std(parms)
disp('10th and 90th percentiles');
prctile(parms,10)
prctile(parms,90)
