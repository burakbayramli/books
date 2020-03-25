%gradresponsesurfis.m
N = 10000;
theta0 = 3;
a = [theta0,2,3,1,2];
u = rand(N,5);
X = u.*repmat(a,N,1);
X(:,1) = -log(u(:,1))*theta0;
W = zeros(N,1);
Sc = zeros(N,1);
HX = H(X);
theta = 0.1:0.01:theta0*2;
num = numel(theta);
gradell = zeros(1,num); 
gradellL = zeros(1,num);
gradellU = zeros(1,num);
stgradell = zeros(1,num);
for i=1:num
    th = theta(i);
    Sc = (-th + X(:,1))/th^2;
    W = (exp(-(X(:,1)/th))/th)./(exp(-(X(:,1)/theta0))/theta0);
    HWS = H(X).*W.*Sc;
    gradell(i) = mean(HWS);
    stgradell(i) = std(HWS);
    gradellL(i)= gradell(i) - stgradell(i)/sqrt(N)*1.95;
    gradellU(i)= gradell(i) + stgradell(i)/sqrt(N)*1.95;
end
plot(theta,gradell, theta, gradellL, theta, gradellU)

