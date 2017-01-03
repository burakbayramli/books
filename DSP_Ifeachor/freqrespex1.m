%
% m-file to illustrate the computation of frequency response of an IIR filter
% using FFT (freqrespex1.m).
%
Fs=500;								% sampling frequency
b1=[1 -1.6180 1]; b2=[];		% numerator/denominator filter coefficients
a1=[1 -1.5161 0.878]; a2=[];
B=[b1; b2]; A=[a1; a2];
[b,a]=sos2tf([B A]);
n=256;								% number of frequency points
dx=0;
if dx
   freqz(b,a,n,Fs);				% Frequency response evaluation by FFT
else
   f=(0:n-1)*(Fs/2)/n;
   freqz(b,a,f,Fs);				% frequency response by direct evaluation.
end
