function out=asian_option_QMC(n)
% 'n' is the number of trading days;
% output of function is the relative error
% of CMC and QMC for a given 'n';

% option details; make T a function of n
T=n/365; %maturity in n days, which is n/365 of the year
r=.07; % annual interest
sig=0.2; % volatility
K=35; % stike price
S_0=40; % initial stock price
dt=T/n; % time step
% set up the quasi-random numbers
M = 40;
N = 10^4/M;
F = faure(n,n,N-1);
% QMC estimation
for i=1:M
    U = mod(F + repmat(rand(1,n),N,1), 1);
    y(i) = h(U,N,n,T,r,sig,K,S_0,dt);
end
ell = mean(y); %estimate
QMC_RE = std(y)/sqrt(M)/ell; % rel. error
% CMC estimation with N*M points
X=nan(N*M,1); % number of sample path simulations
for i=1:N*M
    X(i)=h(rand(1,n),1,n,T,r,sig,K,S_0,dt);
end
c_0=mean(X);
CMC_RE=std(X)/c_0/sqrt(N*M);
out=[CMC_RE, QMC_RE ]*100;

