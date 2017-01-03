%
% Cascade to parallel conversion example, Example 4.14, p216 (ex414a.m)
%
b=[1, -2, 1, 0, 0];
a=[1, -0.41421, 0.08579, 0.292895, 0.5];
[c, p, k] = residuez(b, a);
c
p
k
b0 = b(3)/a(5)
b01=c(1)+c(2)
b11=-(c(1)*p(2)+c(2)*p(1))
a11=-(p(1)+p(2))
a12=p(1)*p(2)
b02=c(3)+c(4)
b12=-(c(3)*p(4)+c(4)*p(3))
a12=-(p(3)+p(4))
a22=p(3)*p(4)


