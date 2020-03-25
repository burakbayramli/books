%down_and_in_Call_Importance_Sampling.m
r=.07; % annual interest
sig=.2; %stock volatility
K=1.2; % stike price
b=.8; % barrier
S_0=1; % initial stock price
n=180;  % number of stock price observations
T=n/365; % length of observation period (in years)
dt=T/n; % time step
N=10^5; % length of chain
x=[-ones(1,60),ones(1,n-60)]*0.4; % initial starting point
[H,path]=down_in_call(x,dt,r,sig,S_0,K,b); % evaluate H(x)

% now we simulate N sample paths of the stock process
% and compute  averages 
mu=0;paths=0;
for i=1:N
    % apply hit-and-run
    d=randn(1,n); d=d/norm(d);
    lam=-d*x'+randn;
    y=x+lam*d; % make proposal
    % evaluate H(y)
    [H_new,path_new]=down_in_call(y,dt,r,sig,S_0,K,b);
    % accept or reject the proposal
    if rand<min(H_new/H,1)
        x=y;   % update
        H=H_new;   
        path=path_new;
    end
    mu=mu+x/N;          % compute an esitmate of E[X]
    paths=paths+path/N; % compute the average stock price trajectory
    if mod(i,2*10^4)==0  % plot every 10^3-th step of the chain
        plot(0:dt:T,path,0:dt:T,0*path+b,0:dt:T,0*path+K)
        axis([0,T,b-0.1,K+.2]),hold all
        pause(.1)
    end
end
plot(0:dt:T,paths,'r','LineWidth',3) % plot the average price trajectory
figure(2)
plot(mu,'k.'), hold on
% smooth the trajectory of E[X] using a spline
pp = csaps(dt:dt:T,mu,1/(1+(dt*10)^3));
mu_t = fnval(pp,[dt:dt:T]);
plot(mu_t,'r') %plot the smoothed trajectory


 
 
 
 
 
 
N=10^5; X=zeros(N,1); W=X; %preallocate memory
for i=1:N
    z=mu+randn(1,n); % importance sampling 
    [H,path]=down_in_call(z,dt,r,sig,S_0,K,b);
    W(i)=exp(-.5*( z*z'-sum((z-mu).^2)  )); % likelihood ratio
  X(i)=W(i)*exp(-r*T)*H;
end

mean(X)
std(X)/mean(X)/sqrt(N)

 
  
%  9.3894e-008  ,0.03856
%  1.02166369865641e-007     1.18867831902203e-007

% 
% x=[ones(1,60)*1.7,-ones(1,n-60)*1.9];
% [H,path]=down_in_call(x,dt,r,sig,S_0,K,b);
% plot(path),H
%pp = spaps([dt:dt:T],mu,sum(var1),ones(1,n));
