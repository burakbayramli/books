%discIT.m
p = [0.2,0.3,0.1,0.05,0.35];
N = 10^5;
x = zeros(N,1);
for i=1:N
    x(i) = min(find(rand<cumsum(p))); %draws from p
end
freq = hist(x,1:5)/N

