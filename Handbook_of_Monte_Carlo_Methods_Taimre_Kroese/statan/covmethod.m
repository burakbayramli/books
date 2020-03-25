%covmethod.m
N=10^5; %sample size, including burn-in
tstat = 300; %burn-in period and maximal lag
sample = zeros(N,1);
sigma = 0.2; %standard deviation of proposal
X = randn*sigma; %generate an initializing point
for k=1:N
    Y = X + randn*sigma; %generate the proposal move
    if rand<min(exp(-.5*(Y-10)^2+.5*(X-10)^2), 1) %acceptance step
        X = Y; %update X
    end
    sample(k) = X; %store sample
end
K = tstat; x = sample(tstat:N);
[R,lags] = xcov(x,K,'unbiased'); %calculate covariance function
plot(lags(K+1:2*K),R(K+1:2*K),'.') %plot covariance function
S2 = R(K+1) + 2*sum(R(K+2:2*K)); %asymptotic variance
ell = mean(x);
RE = sqrt(S2)/ell/sqrt(numel(x));
fprintf('ell = %g ; CI = ( %g , %g ) \n',...
    ell,ell*(1-1.96*RE),ell*(1+1.96*RE))