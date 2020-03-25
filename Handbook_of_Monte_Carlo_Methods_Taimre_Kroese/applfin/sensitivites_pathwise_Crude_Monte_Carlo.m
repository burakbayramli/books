% sensitivities_pathwise_Crude_Monte_Carlo.m
r=.07; % annual interest
sig=0.2; % volatility
K=35; % stike price
S_0=40; % initial stock price
T=4/12; %maturity in 4 months, which is 4/12 of the year
n= 88;  % there are approx. 88 trading days in 4 months
dt=T/n; % time step
% Simulate N sample paths of the stock process
N=10^6; Delta=nan(N,1);Vega=Delta;
for i=1:N
    path=(r-sig^2/2)*dt+sig*sqrt(dt)*randn(1,n);
    path=cumprod([S_0,exp(path)]);
    A=mean(path); % average path/stock price
    Delta(i)=exp(-r*T)*(A>=K)*A/S_0;
    Vega(i)=exp(-r*T)*(A>=K)*...
        sum(  path.*(log(path/S_0)-(r+.5*sig^2)*[0:dt:T])  )/(n+1)/sig;
end
D=mean(Delta); Rel_err=std(Delta)/sqrt(N)
width=std(Delta)*norminv(0.975)/sqrt(N);
CI_D=[D-width, D+width]
V=mean(Vega); Rel_err=std(Vega)/sqrt(N)
width=std(Vega)*norminv(0.975)/sqrt(N);
CI_V=[V-width, V+width]

