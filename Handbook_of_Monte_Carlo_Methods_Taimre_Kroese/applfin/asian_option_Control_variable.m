% asian_option_Control_variable.m
r=.07; % annual interest
sig=0.2; % volatility
K=35; % stike price
S_0=40; % initial stock price
T=4/12; % maturity in 4 months, which is 4/12 of the year
n= 88;  % there are approx. 88 trading days in 4 months
dt=T/n; % time step
% Simulate N sample paths of the stock process
N=10^4; X=nan(N,1); tX=X; % number of sample path simulations
for i=1:N
    path=(r-sig^2/2)*dt+sig*sqrt(dt)*randn(1,n);
    path=cumprod([S_0,exp(path)]);
    X(i)=exp(-r*T)*max(mean(path)-K,0);
    tX(i)=exp(-r*T)*max(prod(path)^(1/(n+1))-K,0);
end
c_0=mean(X)
width=std(X)*norminv(0.975)/sqrt(N);
CI=[c_0-width, c_0+width]

% compute expectation of control variable
a1=(log(S_0/K)+(r-sig^2/6+sig^2/3)*T/2)/sig/sqrt(T/3);
a2=(log(S_0/K)+(r-sig^2/6-sig^2/3)*T/2)/sig/sqrt(T/3);
geo_call=exp(-(6*r+sig^2)*T/12)*S_0*normcdf(a1)-...
         K*exp(-r*T)*normcdf(a2);

Cov=cov([X,tX]);
alpha=Cov(1,2)/Cov(1,1); % optimal linear control

ell_c=c_0-alpha*mean(tX-geo_call)
width=1.96*sqrt((1-Cov(1,2)^2/Cov(1,1)/Cov(2,2))/N*Cov(1,1));
CI=[ell_c-width,ell_c+width]
