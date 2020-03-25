%stateda.m
N = 10^3;
x = -log(rand(1,N)); %the data
y = -log(rand(1,N)); %the data
z = x + y;
subplot(2,2,1), hist(z,20);
[f,xi] = ksdensity(z); %matlab's kernel density function
subplot(2,2,2), plot(xi,f);
subplot(2,2,3), ecdf(z); %empirical cdf
subplot(2,2,4), scatter(x,z,'.');

