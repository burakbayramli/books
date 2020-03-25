%lhcsbridge.m
d = 5;
K = 50;
n = 10^4/K;
est = zeros(n,1);
for i= 1:n
    U = rand(K,d);
    [x,p] = sort(rand(K,d));
    V = (p - 1 + U)/K;
    est(i) = mean(h(V));
end
mean(est)
percRE =  std(est)/sqrt(n)/mean(est)*100
