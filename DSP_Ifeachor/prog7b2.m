%
%	prog7b2.m, p442						File name
%
FS=1000;										%	Sampling frequency
FN=FS/2;										%	Nyquist frequency
N=73;											%	Filter length
beta=5.65;									%	Kaiser ripple parameter
fc1=125/FN;									%	Normalized cut off frequencies
fc2=275/FN;
FC=[fc1 fc2];								%	Band edge frequencies
hn=fir1(N-1, FC, kaiser(N, beta));	%	Obtain windowed filter coefficients
[H,f]=freqz(hn, 1, 512, FS);			%	Compute frequency response
mag=20*log10(abs(H));
plot(f, mag), grid on
xlabel('Frequency (Hz)')
ylabel('Magnitude Response (dB)')

