%
% Program 8.3, p512. m-file for the design of the BZT filter  
% (Example 8.19)
% program name: prog83.m
%
Fs=1000;							% sampling frequency
FN=Fs/2;
fc=300;							% cutoff frequency
N=5;								
[z, p, k]=butter(N, fc/FN);		
[h, f]=freqz(k*poly(z), poly(p), 512, Fs);
plot(f, 20*log10(abs(h))), grid
ylabel('Magnitude (dB)')
xlabel('Frequency (Hz)')

