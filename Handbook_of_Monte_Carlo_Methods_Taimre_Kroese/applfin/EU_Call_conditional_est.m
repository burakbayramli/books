% EU_Call_conditional_est.m
r=0.05; sig_0=1; % volatility
alpha=10;  sig=1; % long term volatility
K=0.85; % stike price
S_0=1; V_0=0; % initial stock and volatility
T=90/365; %maturity in 90 days
n= 90;  % there are approx. 90 trading days in 4 months
dt=T/n; % time step

% now we simulate N sample paths of the stock process
N=10^3; X=nan(N,1);
for k=1:N
    V=nan(1,n); V(1)=V_0;
    for i=1:n-1
        V(i+1)=V(i)+dt*alpha*(sig-V(i))+sqrt(dt)*sig_0*V(i)*randn;
    end
    sigma_bar=sqrt(sum(V.^2)*dt/T);
    X(k)=BS(S_0,K,r,T,sigma_bar); %conditional estimator
end
c_0=mean(X), Rel_error=std(X)/c_0/sqrt(N)
width=std(X)*norminv(0.975)/sqrt(N)
CI=[c_0-width,c_0+width] %95% confidence interval

