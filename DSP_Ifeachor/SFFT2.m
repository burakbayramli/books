%
% 4-point FFT (sfft2.m)
% A simple m-file script illustrating direct 4-point FFT decimation-in-time
% computation.
% Input data: x(0)=1, x(1)=0.5, x(2)=0, x(3)=0.
%
N=4;j=sqrt(-1);
x0=1; x1=0.5; x2=0; x3=0;
W0=1; W1=-j;
a11=x0+W0*x2;b11=x0-W0*x2;
a12=x1+W0*x3; b12=x1-W0*x3;
X0=a11+W0*a12; X2=a11-W0*a12;
X1=b11+W1*b12; X3=b11-W1*b12;
a11
b11
a12
b12
X0
X1
X2
X3
