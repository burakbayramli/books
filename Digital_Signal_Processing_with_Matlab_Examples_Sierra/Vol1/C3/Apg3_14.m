%PSDs of random u(t) and output y(t)
G=tf([100],[1 100]); %the linear system
fs=200; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(8-tiv); %time intervals set (1600 values)
N=length(t); %number of data points
u=randn(N,1); %random input signal data set
[y,ty]=lsim(G,u,t); %random output signal data set
nfft=256; %length of FFT
window=hanning(256); %window function
numoverlap=128; %number of samples overlap
subplot(2,1,1); pwelch(u,window,numoverlap,nfft,fs); %PSD of the input
title('PSDs of input and output'); ylabel('input');
subplot(2,1,2); pwelch(y,window,numoverlap,nfft,fs); %PSD of the output
title(''); ylabel('output');


