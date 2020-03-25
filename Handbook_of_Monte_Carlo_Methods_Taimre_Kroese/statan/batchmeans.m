%batchmeans.m
clear all
p=0.25;q=1-p;M=100000;
K = 100; %throw away
B = 300; %number of batches
N = M-K; %remaining samples
T = (M-K)/B; x = zeros(1,M);
for i=2:M
    x(i)=max(0,x(i-1)+2*(rand<p)-1);
end
y=zeros(1,B); %the batch means
for k=1:B
    y(k) = mean(x(K+1 + (k-1)*T : K + k*T));
end
ell = mean(y);
RE = std(y)/ell/sqrt(B);
true_ell = p/(q-p);
fprintf('true_ell=%g; ell=%g; CI=(%g,%g) \n',...
    true_ell, ell, ell*(1-1.96*RE), ell*(1+1.96*RE))
