%Coherence between system output and noise input
%DTrF case 2
% continuous time transfer function:
num2Ns=[10 0]; den2Ns=[1 3 10]; %G2(s)=10s/(s^2+3s+10);
fs=300; %sampling frequency in Hz
%discrete transfer function (from the continuous case)
[num2Nz,den2Nz]= impinvar(num2Ns,den2Ns,fs); %G2(z)

%response of G2 to noise
tiv=1/fs; %time interval between samples;
t=0:tiv:(60-tiv); %time intervals set (60 seconds)
N=length(t); %number of data points
u=randn(N,1); %random input signal data set
y=filter(num2Nz,den2Nz,u); %G2(z) response to noise

[chr fchr]=cohere(u,y,fs); %coherence computation

%display coherence
figure(1)
plot((fchr*fs/2),chr,'k');
xlabel('Hz');
axis([0 150 0 1.2]);
title('coherence of input and output signals: case 2')
