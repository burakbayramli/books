%resampratio.m
n = 100; %size of data
N = 50000; %resample size
T = zeros(1,N);
hold on
for count=1:3 %generate three sets of data
    xorg = 10 + 5*randn(1,n); %original x data
    yorg = rand(1,n).*xorg; %original y data
    x = zeros(1,n);
    y = zeros(1,n);
    T = zeros(1,N); %bootstrap values for ratio estimator
    for i=1:N
        ind = ceil(n*rand(1,n)); % draw random indices
        x = xorg(ind); % resampled y data
        y = yorg(ind); % resampled x data
        T(i) = mean(x)/mean(y);
    end
    Torg = mean(xorg)/mean(yorg);
    cv = cov(xorg,yorg);
    S2 = Torg^2*(var(xorg)/mean(xorg)^2 ...
       + var(yorg)/mean(yorg)^2 - 2*cv(1,2)/mean(xorg)/mean(yorg));
    tt = [Torg-4*sqrt(S2/n):0.01: Torg+4*sqrt(S2/n)];
    z = normpdf(tt,Torg,sqrt(S2/n));
    plot(tt,z,'r-.')
    [bandwidth,density,xmesh]=kde(T,2^14,min(tt),max(tt));
    plot(xmesh,density)
end
hold off

