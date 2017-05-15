% Strange model from frequency response of transfer function with delay
%case 1d
Td=0.5; %pure delay in seconds
% continuous time transfer function:
num1Ns=10; den1Ns=[1 10]; %G1(s)=10/(s+10)

%frequency response
wr=logspace(0,2,200); %frequency values for response (rad/s)
H1Ns=freqs(num1Ns,den1Ns,wr); %G1(s) frequency response
H1Nsd=H1Ns.*exp(-j*Td*wr); %adding delay to G1(s)

%using invfreqs to obtain the TrF
na=24; %denominator degree
nb=20; %numerator degree
[num1Es,den1Es]=invfreqs(H1Nsd,wr,nb,na); %TrF computation

H1Es=freqs(num1Es,den1Es,wr); %^G1(s) frequency response

%compare frequency responses of original and estimated TrF
figure(1)
plot(H1Nsd,'xr'); hold on;
plot(H1Es,'k');
x1=-1.2; x2=1.2; y1=-1.2; y2=1.2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('frequency responses of original(x) and estimated (solid) TrF');
xlabel('real'); ylabel('imaginary');

%pole-zero map of ^G1(s)
figure(2)
gz=roots(num1Es); gp=roots(den1Es);
plot(gz,'ok'); hold on; %zeros plot
plot(gp,'xk'); %poles plot
x1=-80; x2=60; y1=-120; y2=120;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('zeros and poles of estimated TrF');
xlabel('real'); ylabel('imaginary');

num1Ns
num1Es

den1Ns
den1Es
