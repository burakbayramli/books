%
%	Program name - REMEZ2.m
%
Fs=50000;								%	Sampling frequency										%	Filter length
Ap=1;										%	Pass band ripple in dB
As=45;									%	Stop band attenuation in dB
M=[0 1 0];						%	Desired magnitude response
F=[10000, 12000, 16000, 18000] ;				%	Band edge frequencies
dp=(10^(Ap/20)-1)/(10^(Ap/20)+1);
ds=10^(-As/20);
dev=[ds dp ds];
[N1, F0, M0, W] = remezord(F, M, dev, Fs);
[b delta] = remez(N1, F0, M0, W);%	Compute the filter coefficients
[H, f] = freqz(b, 1, 1024, Fs);	%	Compute the frequency response
mag = 20*log10(abs(H));				% 	of filter and plot it
plot(f, mag), grid on
xlabel('Frequency (Hz)')
ylabel('Magnitude (dB)')
