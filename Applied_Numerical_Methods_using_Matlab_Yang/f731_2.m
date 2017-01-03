function F=f731_2(a)
%error between the polynomial a(x) and f(x)=1/(1+8x^2)
xx=-2+[0:200]/50; F=polyval(a,xx)-1./(1+8*xx.*xx);
