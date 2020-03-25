%chi2eq.m
clear all, rand('state',1)
N = 10^6; k = 50; p = ones(1,k)/k; %true probabilities
u = rand(1,N); x = zeros(1,k);
for i=1:k
    x(i) = sum( (i-1)/k < u & u< i/k ); %observed count
end
t = sum((x - N*p).^2./(N*p)); %test statistic
pval = 1 - cdf('chi2',t,k-1) % p-value
bar(x,0.5)
