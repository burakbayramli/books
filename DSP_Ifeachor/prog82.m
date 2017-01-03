%
% Program 8.2, p511. m-file for the design of the 
% impulse invariant filter (Example 8.19)
% program name: prog82.m
%
Fs=1000;							% sampling frequency
fc=300;							% cutoff frequency
WC=2*pi*fc;						% cutoff frequency in radian
N=5;								% filter order
[b,a]=butter(N,WC,'s');		% create an analog filter
[z, p, k]=butter(N, WC, 's');		
[bz, az]=impinvar(b,a,Fs);	% determine coeffs of IIR filter
[h, f]=freqz(bz, az, 512,Fs);
plot(f, 20*log10(abs(h))), grid
xlabel('Frequency (Hz)')
ylabel('Magnitude (dB)')
