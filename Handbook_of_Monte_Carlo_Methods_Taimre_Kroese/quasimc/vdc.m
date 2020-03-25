function out = vdc(b,N)
out = zeros(N,1);
numd = ceil(log(N)/log(b));
bb = 1./b.^(1:numd);
a = [];
out(1)=0;
for i=2:N
    a = nbe(a,b);
    fa = fliplr(a);
    out(i) = sum(fa.*bb(1:numel(a)));
end