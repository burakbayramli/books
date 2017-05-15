%Obtain discrete transfer function (DTrF) from impulse response
%DTrF case 2
%no noise
% continuous time transfer function:
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10s/(s^2+3s+10);
fs=1000; %sampling frequency in Hz
Ts=1/fs; %sampling period in seconds
%discrete transfer function (from the continuous case)
[num2Nz,den2Nz]= impinvar(num2Ns,den2Ns,fs); %G2(z)

%impulse response of G2(z)
h2Nz=impz(num2Nz,den2Nz,128,fs);

%using stmcb to obtain the DTrF
na=2; %denominator degree
nb=1; %numerator degree
[num2Ez,den2Ez]=stmcb(h2Nz,nb,na); %DTrF computation

%comparing impulse responses
figure(1)
t=0:Ts:(4-Ts); %sampling times data set (4 second)
Ns=length(t);
h2Nz=impz(num2Nz,den2Nz,Ns); %impulse response of G2(z)
h2Ez=impz(num2Ez,den2Ez,Ns); %impulse response of ^G2(z)
plot(t,h2Nz,'xr',t,h2Ez,'k'); %plots both impulse responses
title('impulse response, G2N(z)as x & G2E(z) solid');
xlabel('seconds');

%comparing frequency responses
figure(2)
wr=logspace(-1,2); %frequency values for response (rad/s)
H2Nz=freqz(num2Nz,den2Nz,wr/(2*pi),fs); %G2(z) frequency response
plot(H2Nz,'xr'); hold on;
H2Ez=freqz(num2Ez,den2Ez,wr/(2*pi),fs); %^G2(z) frequency response
plot(H2Ez,'k');
x1=-1; x2=4; y1=-2; y2=2;
axis([x1 x2 y1 y2]);
plot([x1 x2],[0 0],':g'); %x axis
plot([0 0],[y1 y2],':g'); %y axis
title('complex frequency response, G2N(z)as x & G2E(z) solid');
xlabel('real'); ylabel('imaginary');

num2Nz
num2Ez

den2Nz
den2Ez
