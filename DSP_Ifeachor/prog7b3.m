%
%	Program name - prog7b3.m, p446
%  m-file for calculating optimal FIR filter coefficients and 
%  plotting frequency response
%
Fs=10000;									%	Sampling frequency
N=41;											%	Filter length
WT=[10 3 10];								%	Weights of the deviations in the bands
Hd=[0 0 1 1 0 0];							%	Desired magnitude response in the bands
F=[0 0.1 0.2 0.3 0.4 1];				%	Band edge frequencies
b = remez(N-1, F, Hd, WT);				%	Compute the filter coefficients
[H, f] = freqz(b, 1, 512, Fs);		%	Compute the frequency response
mag = 20*log10(abs(H));					%	of filter and plot it
plot(f, mag), grid on
xlabel('Frequency (Hz)')
ylabel('Magnitude (dB)')

