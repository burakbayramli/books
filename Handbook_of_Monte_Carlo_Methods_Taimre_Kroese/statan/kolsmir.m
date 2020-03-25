%kolsmir.m
N = 1000; %sample size
U=rand(1,N); x = log(U./(1-U))+randn(1,N); %generate sample
x = sort(1./(1+exp(-x)));
i=1:N;
dn_up = max(abs(x-i/N));dn_down = max(abs(x-(i-1)/N));
dn = max(dn_up, dn_down);
kn = sqrt(N)*dn; %KS statistic
k = -20:1:20;
a = (-1).^k.*exp(-2*(k.*kn).^2); %calcuate KS probabilities
p = 1 - sum(a) % return the p-value
%or use matlab Statistics toolbox function kstest
[h,p,ksstat,cv] = kstest(x,[x',x'])
% plot the reduced empirical cdf
stairs([0,x],[0,i/N],'r'), hold on, line([0,1],[0,1])
