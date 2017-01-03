%
%	Program name - REMEZ1.m
%
Fs=15000;									%	sampling frequency
N=41;											%	Filter length
WT=[10 3 10];								%	Weights of the deviations in the bands
Hd=[0 0 1 1 0 0];							%	Desired magnitude response in the bands
F=[0 0.06 0.12 0.14667 0.206667 1];	%	Band edge frequencies
[b, delta] = remez(N-1, F, Hd, WT);	%	Compute the filter coefficients
[H, f] = freqz(b, 1, 512, Fs);		%	Compute the frequency response
mag = 20*log10(abs(H));					%	of filter and plot it
plot(f, mag)
xlabel('Frequency (Hz)')
ylabel('Magnitude (dB)')

