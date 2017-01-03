
% 
%
% 8-point DFT (sfft8.m)
% A simple m-file script illustrating direct 8-point DFT computation.
% Input data: x(0)=4, x(1)=2, x(2)=1, x(3)=4, x(4)=6, x(5)=3, x(6)=5, x(7)=2.
%

j=sqrt(-1);
x0=4; x1=2; x2=1; x3=4; x4=6; x5=3; x6=5; x7=2;
W0=1; W1=sqrt(2)/2 - j*sqrt(2)/2; W2=-j; W3=-sqrt(2)/2 - j*sqrt(2)/2;
%	stage 1
A11=x0+W0*x4;B11=x0-W0*x4;
A12=x2+W0*x6;B12=x2-W0*x6;
A13=x1+W0*x5;B13=x1-W0*x5;
A14=x3+W0*x7;B14=x3-W0*x7;
% stage 2
A21=A11+W0*A12;B21=A11-W0*A12;
A22=B11+W2*B12;B22=B11-W2*B12;
A23=A13+W0*A14;B23=A13-W0*A14;
A24=B13+W2*B14;B24=B13-W2*B14;
% stage 3
X0=A21+W0*A23;X4=A21-W0*A23;
X1=A22+W1*A24;X5=A22-W1*A24;
X2=B21+W2*B23;X6=B21-W2*B23;
X3=B22+W3*B24;X7=B22-W3*B24;
A11
B11
A12
B12
A13
B13
A14
B14
A21
A22
B21
B22
A23
A24
B23
B24
X0
X1
X2
X3
X4
X5
X6
X7

