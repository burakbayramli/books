%sfbridge.m
N = 10^6;
a = [1,2,3,1,2];
u = rand(N,5);
for comp=1:5
    X = u.*repmat(a,N,1);
    hx = H(X);
    X(:,comp) = a(comp);
    hxs = H(X);
    R = (-hx + hxs)/a(comp);
    gr = mean(R);
    se = std(R)/sqrt(N);
    fprintf('%g pm %3.1e\n',  gr, se);
end
