%
% A script illustrating cascade to parallel realization
% (Program 4B.3, p237; program name: prog4d3.m)
%
nstage=2;
N1 =	[1 0.481199 1];
N2 =	[1 1.474597 1];
D1 =	[1 0.052921 0.83173];
D2 =	[1 -0.304609 0.238865];
sos =	[N1 D1; N2 N2];
[b, a] = sos2tf(sos);
[c, p, k] = residue(b, a);
m = length(b);
b0 = b(m)/a(m);
j=1;
for i=1:nstage
   bk(j)=c(j)+c(j+1);
   bk(j+1)=-(c(j)*p(j+1)+c(j+1)*p(j));
   ak(j)=-(p(j)+p(j+1));
   ak(j+1)=p(j)*p(j+1);
   j=j+2;
end   
b0
ak
bk
c
p
k


