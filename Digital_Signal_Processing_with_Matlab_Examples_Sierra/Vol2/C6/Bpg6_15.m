% Transfer function + delay, for study: impulse response
%case 1d
Td=0.5; %pure delay in seconds
% continuous time transfer function:
num1Ns=10; den1Ns=[1 10]; %G1(s)=10/(s+10)
fs=1000; %sampling frequency in Hz
Ts=1/fs; %sampling period in seconds
Nd=ceil(Td/Ts); %number of samples corresponding to the delay
%discrete transfer function (from the continuous cases)
[num1Nz,den1Nz]= impinvar(num1Ns,den1Ns,fs); %G1(z)
num1Nz=[zeros(1,Nd) num1Nz]; %adding delay to G1(z)

%impulse response
t=0:Ts:(4-Ts); %sampling times data set (4 seconds)
Ns=length(t); %number of samples
[h1Nz,t1]=impz(num1Nz,den1Nz,Ns); %G1(z) impulse response
plot(t,h1Nz,'b'); hold on;
title('impulse response of G1(z)');
xlabel('seconds')
axis([0 4 0 0.012]);
