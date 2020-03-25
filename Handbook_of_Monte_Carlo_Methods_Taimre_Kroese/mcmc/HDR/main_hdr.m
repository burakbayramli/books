%main_hdr.m
m=10^4;N=30;gam=1:8;
ell=nan(N,1);
for i=1:N
    c=hdr(gam,m);
    ell(i)=prod(c);
end
mean(ell) % estimate
std(ell)/mean(ell)/sqrt(N) % relative error
