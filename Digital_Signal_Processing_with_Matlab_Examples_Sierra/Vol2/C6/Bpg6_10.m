% Transfer functions for study: impulse response
% continuous time transfer functions:
num1Ns=10; den1Ns=[1 10]; %G1(s)=10/(s+10)
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10s/(s^2+3s+10);

fs=1000; %sampling frequency in Hz
Ts=1/fs; %sampling period in seconds

%discrete transfer functions (from the continuous cases)
[num1Nz,den1Nz]= impinvar(num1Ns,den1Ns,fs); %G1(z)
[num2Nz,den2Nz]= impinvar(num2Ns,den2Ns,fs); %G2(z)

%impulse responses
t=0:Ts:(4-Ts); %sampling times data set (4 seconds)
Ns=length(t); %number of samples
subplot(2,1,1)
[h1Nz,t1]=impz(num1Nz,den1Nz,Ns); %G1(z) impulse response
plot(t,h1Nz,'b'); hold on;
title('impulse response of G1(z)');

subplot(2,1,2)
[h2Nz,t2]=impz(num2Nz,den2Nz,Ns); %G2(z) impulse response
plot(t,h2Nz,'b'); hold on;
title('impulse response of G2(z)');
xlabel('sec')
