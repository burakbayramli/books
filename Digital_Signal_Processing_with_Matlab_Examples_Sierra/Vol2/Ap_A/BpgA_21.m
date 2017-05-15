% Transfer function + delay, for study: impulse response
%case 2d
Td=0.5; %pure delay in seconds
% continuous time transfer function:
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10s/(s^2+3s+10)
fs=1000; %sampling frequency in Hz
Ts=1/fs; %sampling period in seconds
Nd=ceil(Td/Ts); %number of samples corresponding to the delay
%discrete transfer function (from the continuous cases)
[num2Nz,den2Nz]= impinvar(num2Ns,den2Ns,fs); %G2(z)
num2Nz=[zeros(1,Nd) num2Nz]; %adding delay to G2(z)

%impulse response
t=0:Ts:(4-Ts); %sampling times data set (4 seconds)
Ns=length(t); %number of samples
[h2Nz,t1]=impz(num2Nz,den2Nz,Ns); %G2(z) impulse response
plot(t,h2Nz,'b'); hold on;
title('impulse response of G2(z)');
xlabel('seconds');
axis([0 4 -0.005 0.012]);
