%responsesurfis.m
N = 10000; theta0 = 3;
a = [theta0,2,3,1,2]; u = rand(N,5);
X = u.*repmat(a,N,1); W = zeros(N,1);
y = H1(X); theta = 0:0.01:theta0;
num = numel(theta);
ell = zeros(1,num); ellL = zeros(1,num);
ellU = zeros(1,num); stell = zeros(1,num);
for i=1:num
    th = theta(i);
    W = theta0/th*(X(:,1)< th);
    ell(i) = mean(H1(X).*W);
    stell(i) = std(H1(X).*W);
    ellL(i)= ell(i) - stell(i)/sqrt(N)*1.95;
    ellU(i)= ell(i) + stell(i)/sqrt(N)*1.95;
end
plot(theta,ell, theta, ellL, theta, ellU)

