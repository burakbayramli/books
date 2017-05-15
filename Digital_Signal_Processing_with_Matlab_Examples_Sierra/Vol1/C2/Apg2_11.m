% Power spectral density (PSD) of random signal with log-normal PDF
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(40.96-tiv); %time intervals set (4096 values)
N=length(t); %number of data points
mu=0; sigma=1; %random signal parameters
y=lognrnd(mu,sigma,N,1); %random signal data set
nfft=256; %length of FFT
window=hanning(256); %window function
numoverlap=128; %number of samples overlap
pwelch(y,nfft,fs,window,numoverlap);
title('PSD of random signal with log-normal PDF');
