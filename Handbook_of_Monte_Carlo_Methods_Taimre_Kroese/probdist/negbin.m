%negbin.m
alpha = [1 0 0 0];
p = 0.2;
m = 4;
A = [1 - p, p, 0, 0; 0, 1-p ,p ,0 ; 0 ,0, 1-p, p; 0, 0, 0 ,1-p] ;
A0 = abs((eye(m) - A)*ones(m,1));
P = [A A0; zeros(1,m) 1]
N = 10^5;
x = zeros(N,1);  
for i=1:N;
    X = 0;
    Y = min(find(cumsum(alpha)> rand));
    while Y ~= m+1
        Y = min(find(cumsum(P(Y,:))> rand));
        X = X+1;
    end
    x(i) = X;
end
x = x - m; % shifted samples come from a NegBin(n,p) distribution.  

xx = [0: 2*m/p];
count = hist(x,xx);
ex = nbinpdf(xx,m,p)*N;
hold on
plot(xx,count,'.r')
plot(xx,ex,'ob')
hold off
