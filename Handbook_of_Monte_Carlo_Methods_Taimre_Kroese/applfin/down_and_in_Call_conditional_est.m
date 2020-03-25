%down_and_in_Call_conditional_est.m
r=.07; % annual interest
sig=2; %stock volatility
K=1.1; % stike price
b=.9; % barrier
S_0=1; % initial stock price
n=180;  % number of stock price observations
T=n/365; % length of observation period (in years)
dt=T/n; % time step
% simulate N sample paths of the stock process
N=10^5; X=zeros(N,1); % number of sample path simulations
for i=1:N
    path=(r-sig^2/2)*dt+sig*sqrt(dt)*randn(1,n);
    path=cumprod([S_0,exp(path)]);
    index=find(path(1:end-1)<=b,1,'First'); % index of first breach of barrier
    if ~isempty(index)
        tau=dt*(index-1); S_tau=path(index);
        X(i)=BS(S_tau,K,r,T-tau,sig);
    end
end
c_0=mean(X), Rel_error=std(X)/c_0/sqrt(N)
width=std(X)*norminv(0.975)/sqrt(N)
CI=[c_0-width,c_0+width]

