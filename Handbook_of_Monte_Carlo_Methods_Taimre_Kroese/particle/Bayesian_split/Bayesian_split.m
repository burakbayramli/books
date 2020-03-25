clear all,clc
global Y X
n=5000; % number of data points (y_1,...,y_n)
k=3;   % number of explanatory variables
% generate artificial dataset
randn('state', 12345);  rand('state', 67890);
truebeta = [1 -5.5 1]';
X = [ ones(n,1) randn(n,k-1)*0.1 ];  % design matrix
Y = binornd(1,1./(1+exp(-X*truebeta)));
bo=zeros(k,1); % we use Vo=100*eye(k);
% determine the Maximum Likelihood using Newton Raphson
err=inf; b=bo; % initial guess
while norm(err)>10^(-3)
    p=1./(1+exp(-X*b));
    g=X'*(Y-p);
    H=-X'*diag(p.^2.*(1./p - 1))*X;
    err=H\g; % compute Newton-Raphson correction
    b=b-err; % update Newton guess for MLE
end

log_H_b=S([b',1]); %logarithm of MLE
N=10^3; rho=0.1; Gamma=log_H_b+5; %conservative upper bound
% GS algorithm starts here
for k=1:10
    [el,gam,beta]=adam(N,Gamma,rho); ell(k)=el;
end
RE=std(ell)/mean(ell)/sqrt(10)
log_Z=Gamma+log(mean(ell))
%95% confidence interval
[log_Z-1.96*RE,log_Z+1.96*RE]






















% Xx=nan(1,10^6);
% for i=1:10^6
%
%   Xx(i)=exp(S([randn(1,3)*10,1])-Gamma);
% end
% Xx(isnan(Xx))=[];
% Gamma+log(mean(Xx))  % variance via delta method =0.0079^2
% % hence RE = 8.6485e-005
%  -91.3450547670315
