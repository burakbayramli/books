%geometricbm.m
T=1; % final time
n=10000; h=T/(n-1); t= 0:h:T;
mu = 1; sigma = 0.2; xo=1; % parameters
W = sqrt(h)*[0, cumsum(randn(1,n-1))];
x = xo*exp((mu - sigma^2/2)*t + sigma*W);
plot(t,x)
hold on
plot(t, exp(mu*t),'r');
   
