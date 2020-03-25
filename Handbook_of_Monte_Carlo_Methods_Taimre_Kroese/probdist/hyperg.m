%hyperg.m
N = 100; %total number of balls
n = 20; % take n balls
r = 30; % number of red balls
w = zeros(1,N);
w(1:r) = 1;
K = 10^5;
x = zeros(1,K);
for i=1:K
    [s,ix] = sort(rand(1,N)); 
    x(i) = sum(w(ix(1:n)));
end

% Uncomment for plotting purposes
%xx = [0:n];
%count = hist(x,xx);
%ex = hygepdf(xx,N,r,n)*K;
%clf
%hold on
%plot(xx,count,'.r')
%plot(xx,ex,'ob')
%hold off
