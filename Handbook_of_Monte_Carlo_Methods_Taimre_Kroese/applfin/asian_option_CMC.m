%asian_option_CMC.m
r=.07; % annual interest
sig=0.2; % volatility
K=35; % stike price
S_0=40; % initial stock price
T=4/12; %maturity in 4 months, which is 4/12 of the year
n= 88;  % there are approx. 88 trading days in 4 months
dt=T/n; % time step
% simulate N sample paths of the stock process
N=10^4; X=nan(N,1); % number of sample path simulations
for i=1:N
    path=(r-sig^2/2)*dt+sig*sqrt(dt)*randn(1,n);
    path=cumprod([S_0,exp(path)]);
    X(i)=exp(-r*T)*max(mean(path)-K,0);
end
c_0=mean(X), Rel_error=std(X)/c_0/sqrt(N)
width=std(X)*norminv(0.975)/sqrt(N)
CI=[c_0-width,c_0+width]
