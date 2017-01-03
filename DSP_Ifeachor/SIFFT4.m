%
% 4-point inverse FFT (sifft4.m)
% A simple m-file script illustrating direct 4-point inverse DFT computation.
% Input data: x(0)=1.5 +0j, x(1)=1-0.5j, x(2)=0.5+0j, x(3)=1+0.5j
%
N=4;j=sqrt(-1);
x0=1.5 + 0j; x1=1 - 0.5j; x2=0.5 + 0j; x3=1+0.5j;
W0=1; W1=j;
a11=x0+W0*x2;b11=x0-W0*x2;
a12=x1+W0*x3; b12=x1-W0*x3;
X0=(a11+W0*a12)/N; X2=(a11-W0*a12)/N;
X1=(b11+W1*b12)/N; X3=(b11-W1*b12)/N;
a11
b11
a12
b12
X0
X1
X2
X3
x0
x1
x2
x3