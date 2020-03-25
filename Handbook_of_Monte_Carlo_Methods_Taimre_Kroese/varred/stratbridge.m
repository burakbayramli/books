%stratbridge.m
K = 4;
m = K^5; %number of strata
N = 10^4; %total number of samples
n = ceil(N/m); %number of samples per stratum
est = zeros(n,1);
R=(1:m)';
W=zeros(m,5);
W(:,1)=mod(R,K);
for i=2:5
    W(:,i)=(mod(R,K^i)-mod(R,K^(i-1)))./(K^(i-1));
end

for j=1:n
    V=(W+rand(m,5))./K;
    est(j)=mean(h(V));
end
mest = mean(est)
percRE = std(est)/sqrt(n)/mest*100
