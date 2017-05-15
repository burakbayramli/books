% Transfer functions for study: frequency response
% continuous time transfer functions:
num1Ns=10; den1Ns=[1 10]; %G1(s)=10/(s+10)
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10s/(s^2+3s+10);

fs=1000; %sampling frequency in Hz
Ts=1/fs; %sampling period in seconds

%discrete transfer functions (from the continuous cases)
[num1Nz,den1Nz]= impinvar(num1Ns,den1Ns,fs); %G1(z)
[num2Nz,den2Nz]= impinvar(num2Ns,den2Ns,fs); %G2(z)

%frequency responses
wr=logspace(-1,2); %frequency values for response (rad/s)
subplot(2,1,1)
H1Ns=freqs(num1Ns,den1Ns,wr); %G1(s) frequency response
semilogx(wr,abs(H1Ns),'k'); hold on;
H1Nz=freqz(num1Nz,den1Nz,wr/(2*pi),fs); %G1(z) frequency response
semilogx(wr,abs(H1Nz),'xb');
title('G1(s) as solid & G1(z) as x');
axis([0.1 100 0 1.2]);

subplot(2,1,2)
H2Ns=freqs(num2Ns,den2Ns,wr); %G2(s) frequency response
semilogx(wr,abs(H2Ns),'k'); hold on;
H2Nz=freqz(num2Nz,den2Nz,wr/(2*pi),fs); %G2(z) frequency response
semilogx(wr,abs(H2Nz),'xb');
title('G2(s) as solid & G2(z) as x');
xlabel('rad/s')
axis([0.1 100 0 4]);
