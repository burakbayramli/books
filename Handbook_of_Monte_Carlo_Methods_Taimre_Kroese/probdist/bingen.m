%bingen.m
n = 100; p = 0.1; mu = n*p; N = 10^5;
x = zeros(1,N); c = log(1-p);
for i=1:N
    s = ceil(log(rand)/c);
    xi = 0;
    while s < n + 1
       xi = xi+ 1;
       s = s + ceil(log(rand)/c);
    end
    x(i)= xi;
end
  
xx = [floor(mu - 4*sqrt(mu)):1:ceil(mu + 4*sqrt(mu))];
count = hist(x,xx);
ex = binopdf(xx,n,p)*N;
hold on
plot(xx,count,'or')
plot(xx,ex,'.b')
hold off
