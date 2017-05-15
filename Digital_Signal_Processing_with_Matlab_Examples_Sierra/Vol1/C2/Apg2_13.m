% Power spectral density (PSD) of a signal+noise
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(40.96-tiv); %time intervals set (4096 values)
N=length(t); %number of data points
yr=randn(N,1); %random signal data set
ys=sin(15*2*pi*t); %sinusoidal signal (15 Hz)
y=ys+yr'; %the signal+noise
nfft=256; %length of FFT
window=hanning(256); %window function
numoverlap=128; %number of samples overlap
pwelch(y,nfft,fs,window,numoverlap);
title('PSD of a sine+noise signal');
