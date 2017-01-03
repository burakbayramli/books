%
% 4-point DFT (sdft2.m)
% A simple m-file script illustrating direct 4-point DFT computation.
% input data: x(0)=1, x(1)=0.5, x(2)=0, x(3)=0.
% 
N=4;j=sqrt(-1);
x0=1; x1=0.5; x2=0; x3=0;
X0=x0+x1+x2+x3;
X11=x1*exp(-j*2*pi/N);
X12=x2*exp(-j*2*pi*2/N);
X13=x3*exp(-j*2*pi*3/N);
X1a=x0+X11+X12+X13;
X1=x0+x1*exp(-j*2*pi/N)+x2*exp(-j*2*pi*2/N)+x3*exp(-j*2*pi*3/N);
X2=x0+x1*exp(-j*2*pi*2/N)+x2*exp(-j*2*pi*2*2/N)+x3*exp(-j*2*pi*2*3/N);
X3=x0+x1*exp(-j*2*pi*3/N)+x2*exp(-j*2*pi*3*2/N)+x3*exp(-j*2*pi*3*3/N);
X11
X12
X13
X1a
X0
X1
X2
X3
