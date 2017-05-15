% Transfer function + delay, for study: noise response
%case 2d
Td=0.5; %pure delay in seconds
% continuous time transfer function:
num2Ns=10; den2Ns=[1 10]; %G2(s)=10/(s+10)
fs=1000; %sampling frequency in Hz
Ts=1/fs; %sampling period in seconds
Nd=ceil(Td/Ts); %number of samples corresponding to the delay
%discrete transfer function (from the continuous cases)
[num2Nz,den2Nz]= impinvar(num2Ns,den2Ns,fs); %G2(z)
num2Nz=[zeros(1,Nd) num2Nz]; %adding delay to G2(z)

%response of G2 to noise
tiv=1/fs; %time interval between samples;
t=0:tiv:(10-tiv); %time intervals set (10 seconds)
N=length(t); %number of data points
u=randn(N,1); %random input signal data set
y=filter(num2Nz,den2Nz,u); %G2(z) response to noise

ac=xcorr(u,y); %cross-correlation of u and y
ac=ac/N; %normalization
Nac=length(ac); mNac=ceil(0.5*Nac); %to plot half ac

%display cross-correlation
plot(t,ac(mNac:Nac),'k');
grid;
title('G2(z)+delay noise response cross-correlation');
xlabel('seconds');
