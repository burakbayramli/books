%ipabridge.m
N = 10^6;
a = [1,2,3,1,2];
A = [1,2;3,4;2,3;1,3;2,4];
u = rand(N,5);
for comp=1:5
    dh = zeros(N,1);
    [y,K] = HK(u,a);
    ind = find(K == A(comp,1) | K==A(comp,2));
    dh(ind) = u(ind,comp);
    gr = mean(dh);
    se = std(dh)/sqrt(N);
    fprintf('%g pm %3.1e\n',  gr, se);
end


