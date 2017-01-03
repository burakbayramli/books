function DFTD

clear all;
% Program to compute DFT coefficients directly (dftd.m)
% This program is the same as Program 3c.1 in the main text
%
direction = -1;	%1 - forward DFT, -1 - inverse DFT
in = fopen('datain.dat','r');
x = fscanf(in,'%g %g',[2,inf]);
fclose(in);
x = x(1,:)+x(2,:)*i;		% form complex numbers

if direction==1 
	y = x*dftmtx(length(x)) ;	%compute DFT
else
	y = x*conj(dftmtx(length(x)))/length(x);	%compute IDFT
end

% Save/Print the results
out=fopen('dataout.dat','w');
fprintf(out,'%g %g\n',[real(y); imag(y)]);
fclose(out);
subplot(2,1,1),plot(1:length(x),x); title('Input Signal');
subplot(2,1,2),plot(1:length(y),y); title('Output Signal');

======================================================

function DFTF								
%	Program to compute DFT coefficients using DIT FFT		
%	This program is the same as Program 3c.2 in the main text (dftf.m)
%
clear all;
direction = -1;	%1 - forward DFT, -1 - inverse DFT
in = fopen('dataout.dat','r');
x = fscanf(in,'%g %g',[2,inf]);
fclose(in);
x = x(1,:)+x(2,:)*i;		% form complex numbers

if direction==1 
	y=fft(x,length(x))		% compute FFT
else
	y=ifft(x,length(x))		% compute IFFT
end

% Save/Print the results
out=fopen('dataout.dat','w');
fprintf(out,'%g %g\n',[real(y); imag(y)]);
fclose(out);
subplot(2,1,1),plot(1:length(x),x); title('Input Signal');
subplot(2,1,2),plot(1:length(y),y); title('Output Signal');

=======================================================

%
% 4-point DFT (sdft1.m)
% A simple m-file script illustrating direct 4-point DFT computation.
% input data: x(0)=1, x(1)=5, x(2)=4, x(3)=2.
%
N=4;j=sqrt(-1);
x0=1; x1=5; x2=4; x3=2;
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

========================================================

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

================================================================

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

========================================================

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

=================================================================

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


