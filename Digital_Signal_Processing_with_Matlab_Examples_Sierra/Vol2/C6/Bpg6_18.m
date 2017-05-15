% Transfer function + delay, for study: noise response
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

%response of G1 to noise
tiv=1/fs; %time interval between samples;
t=0:tiv:(10-tiv); %time intervals set (10 seconds)
N=length(t); %number of data points
u=randn(N,1); %random input signal data set
y=filter(num1Nz,den1Nz,u); %G1(z) response to noise

ac=xcorr(u,y); %cross-correlation of u and y
ac=ac/N; %normalization
Nac=length(ac); mNac=ceil(0.5*Nac); %to plot half ac

%display cross-correlation
plot(t,ac(mNac:Nac),'k');
grid;
title('G1(z)+delay noise response cross-correlation');
xlabel('seconds');
