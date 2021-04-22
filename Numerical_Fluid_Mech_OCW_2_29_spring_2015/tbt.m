% Solves 2x2 system of linear equations
% using gaussian elimination. 2 digit arithmetic
n=2
a = [ [0.01 1.0]' [-1.0 0.01]']
b= [1 1]'
r=a^(-1) * b

x=[0 0];
m21=a(2,1)/a(1,1);
a(2,1)=0;
a(2,2) = radd(a(2,2),-m21*a(1,2),n);
b(2)   = radd(b(2),-m21*b(1),n);

x(2)   = b(2)/a(2,2);
x(1)   = (radd(b(1), -a(1,2)*x(2),n))/a(1,1);
x'


