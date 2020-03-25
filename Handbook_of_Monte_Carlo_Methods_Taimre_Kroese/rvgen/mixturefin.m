%mixturefin.m
p = [0.2, 0.4, 0.4];
mu = [-0.5, 1, 2];
sigma = [0.5, 0.4, 2];
N = 10^5
[dummy,t]=histc(rand(1,N),[0,cumsum(p)]); % draw from p
x = randn(1,N).*sigma(t) + mu(t);    % draw a normal r.v.      
hist(x,200)                          % make a histogram of the data

