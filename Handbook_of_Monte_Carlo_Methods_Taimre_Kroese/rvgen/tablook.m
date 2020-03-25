%tablook.m
r = 10;
A = zeros(1,(r+1)*r/2);
n=0;
for i=1:r
    for j=1:i
        n = n+1;
        A(n) = i;
    end
end
I = ceil(rand(1,10^5)*n);
X = A(I);

hist(X,1:r)
