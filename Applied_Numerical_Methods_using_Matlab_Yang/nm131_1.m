%nm131_1: nested multiplication vs. plain multiple multiplication
a=[1:5]; N=length(a); x=1;
tic
p=sum([a(1)*x^4 a(2)*x^3 a(3)*x^2 a(4)*x a(5)]);%plain multiplication
p, toc
tic, pn=a(1);
for i=2:N %nested multiplication
   pn= pn*x +a(i);
end
pn, toc
tic, polyval(a,x), toc

%More generally
N=1000000; a=[1:N]; x=1;
tic
p=sum(a.*x.^[N-1:-1:0]);%plain multiplication
p, toc
tic, pn=a(1);
for i=2:N %nested multiplication
   pn= pn*x +a(i);
end
pn, toc
tic, polyval(a,x), toc