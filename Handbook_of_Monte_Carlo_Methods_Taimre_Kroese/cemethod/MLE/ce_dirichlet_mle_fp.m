%MLE\ce_dirichlet_mle_fp.m
clear all;
a=(1:1:5); n=100;
K=length(a); %K dim vector
data=dirichletrnd(a,n,K); % Generate data
epsilon=10^(-4); % For 
N=10^4; rho=0.1; alphamu=1; alphasig=0.5; Ne=ceil(N.*rho);
mu=zeros(1,K); sig=ones(1,K).*10; % Initial parameters
muhist=mu;sighist=sig; % History
while max(sig)>epsilon
    x=repmat(mu,N,1)+repmat(sig,N,1).*randn(N,K); % Sample
    S=dirichlet_log_like(data,x,n,K); [S,I]=sort(S); % Score & Sort
    mu=alphamu.*mean(x(I(N-Ne+1:N),:))+(1-alphamu).*mu;
    sig=alphasig.*std(x(I(N-Ne+1:N),:),1,1)+(1-alphasig).*sig;
    muhist=[muhist;mu];sighist=[sighist;sig]; % Update History
    [mu, sig, S(end),S(N-Ne+1)]
end
% For comparison, compute the MLE using a fixed-point method
afp=dirichlet_MLE_FP(data,K);
disp([afp,dirichlet_log_like(data,afp,n,K)])
