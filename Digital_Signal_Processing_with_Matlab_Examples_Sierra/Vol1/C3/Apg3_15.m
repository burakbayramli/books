%Estimate of the transfer function from random input u(t) and output y(t)
num=100; den=[1 100];
G=tf(num,den); %the linear system
fs=200; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(8-tiv); %time intervals set (1600 values)
N=length(t); %number of data points
u=randn(N,1); %random input signal data set
[y,ty]=lsim(G,u,t); %random output signal data set
nfft=256; %length of FFT
window=hanning(256); %window function
numoverlap=128; %number of samples overlap

%Frequency response of the system G(s):
Hz=0:0.1:100; w=2*pi*Hz; %frequencies
G=freqs(num,den,w); %frequency response of the system
subplot(2,1,1); plot(Hz,abs(G),'k') %plots frequency response of the system
title('Real and estimated frequency response of G(s)'); ylabel('the system'); xlabel('Hz')
%Frequency response of the transfer function estimate:
[GE,FE]=tfe(u,y,nfft,fs,window,numoverlap); %frequency response estimate
subplot(2,1,2); plot(FE,abs(GE),'k'); %plots estimated frequency response of the system
title(''); xlabel('Hz'); ylabel('the estimate');


