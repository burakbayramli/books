%Obtain discrete transfer function (DTrF) from discrete frequency response
%DTrF case 1
%no noise
% continuous time transfer function:
num1Ns=10; den1Ns=[1 10]; %G1(s)=10/(s+10)
fs=1000; %sampling frequency in Hz
Ts=1/fs; %sampling period in seconds
%discrete transfer function (from the continuous case)
[num1Nz,den1Nz]= impinvar(num1Ns,den1Ns,fs); %G1(z)

wr=logspace(-1,2); %frequency values for response (rad/s)

%discrete frequency response of G1(z)
H1Nz=freqz(num1Nz,den1Nz,wr/(2*pi),fs); %G1(z) frequency response

%using invfreqz to obtain the DTrF
na=1; %denominator degree
nb=0; %numerator degree
W=wr/fs; %normalized frequency values 0..pi rad/s
[num1Ez,den1Ez]=invfreqz(H1Nz,W,nb,na); %DTrF computation

H1Ez=freqz(num1Ez,den1Ez,wr/(2*pi),fs); %^G1(z) frequency response

%comparing frequency responses
plot(H1Nz,'xr'); hold on;
plot(H1Ez,'k');
x1=-0.2; x2=1.2; y1=-0.6; y2=0.2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('complex frequency response, G1N(z)as x & G1E(z) solid');
xlabel('real'); ylabel('imaginary');

num1Nz
num1Ez

den1Nz
den1Ez
