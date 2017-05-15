%Obtain discrete transfer function (DTrF) from impulse response
%DTrF case 1
%no noise
% continuous time transfer function:
num1Ns=10; den1Ns=[1 10]; %G1(s)=10/(s+10)
fs=1000; %sampling frequency in Hz
Ts=1/fs; %sampling period in seconds
%discrete transfer function (from the continuous case)
[num1Nz,den1Nz]= impinvar(num1Ns,den1Ns,fs); %G1(z)

%impulse response of G1(z)
h1Nz=impz(num1Nz,den1Nz,128,fs);

%using prony to obtain the DTrF
na=1; %denominator degree
nb=0; %numerator degree
[num1Ez,den1Ez]=prony(h1Nz,nb,na); %DTrF computation

%comparing impulse responses
figure(1)
t=0:Ts:(1-Ts); %sampling times data set (1 second)
Ns=length(t);
h1Nz=impz(num1Nz,den1Nz,Ns); %impulse response of G1(z)
h1Ez=impz(num1Ez,den1Ez,Ns); %impulse response of ^G1(z)
plot(t,h1Nz,'xr',t,h1Ez,'k'); %plots both impulse responses
title('impulse response, G1N(z)as x & G1E(z) solid');
xlabel('seconds');

%comparing frequency responses
figure(2)
wr=logspace(-1,2); %frequency values for response (rad/s)
H1Nz=freqz(num1Nz,den1Nz,wr/(2*pi),fs); %G1(z) frequency response
plot(H1Nz,'xr'); hold on;
H1Ez=freqz(num1Ez,den1Ez,wr/(2*pi),fs); %^G1(z) frequency response
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
