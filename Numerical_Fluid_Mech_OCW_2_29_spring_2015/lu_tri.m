function [alf,bet] = lu_tri(a,b,c);
n=length(a);
alf=zeros(n,1);
bet=alf;
alf(1)=a(1);
for k=2:n;
    bet(k) = b(k)/alf(k-1);
    alf(k) = a(k) - bet(k)*c(k-1);
end

        