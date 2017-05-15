% Strange model from frequency response of transfer function with delay
%case 2d
Td=0.5; %pure delay in seconds
% continuous time transfer function:
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10s/(s^2+3s+10)

%frequency response
wr=logspace(0,2,200); %frequency values for response (rad/s)
H2Ns=freqs(num2Ns,den2Ns,wr); %G2(s) frequency response
H2Nsd=H2Ns.*exp(-j*Td*wr); %adding delay to G2(s)

%using invfreqs to obtain the TrF
na=29; %denominator degree
nb=27; %numerator degree
[num2Es,den2Es]=invfreqs(H2Nsd,wr,nb,na); %TrF computation

H2Es=freqs(num2Es,den2Es,wr); %^G2(s) frequency response

%compare frequency responses of original and estimated TrF
figure(1)
plot(H2Nsd,'xr'); hold on;
plot(H2Es,'k');
x1=-4; x2=3; y1=-4; y2=2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('frequency responses of original(x) and estimated (solid) TrF');
xlabel('real'); ylabel('imaginary');

%pole-zero map of ^G2(s)
figure(2)
gz=roots(num2Es); gp=roots(den2Es);
plot(gz,'ok'); hold on; %zeros plot
plot(gp,'xk'); %poles plot
x1=-80; x2=60; y1=-120; y2=120;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('zeros and poles of estimated TrF');
xlabel('real'); ylabel('imaginary');

num2Ns
num2Es

den2Ns
den2Es
