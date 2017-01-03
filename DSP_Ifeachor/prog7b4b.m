%
%	Program name - prog7b4b.m, p450
%  An alternative m-file for calculating the optimal FIR filter 
%  coefficients and plotting frequency response for Example 7B.4
%
N=44;
Fs=50000;								%	Sampling frequency										%	Filter length
Ap=1;										%	Pass band ripple in dB
As=45;									%	Stop band attenuation in dB
M=[0 0 1 1 0 0];						%	Desired magnitude response
F=[0, 0.4, 0.48, 0.64, 0.72 1] ;				%	Band edge frequencies
dp=(10^(Ap/20)-1)/(10^(Ap/20)+1);
ds=10^(-As/20);
W=[dp/ds, 1, dp/ds];
dev=[ds ds dp dp ds ds ];
[b delta] = remez(N-1, F, M, W);%	Compute the filter coefficients
[H, f] = freqz(b, 1, 1024, Fs);	%	Compute the frequency response
mag = 20*log10(abs(H));				% 	of filter and plot it
plot(f, mag), grid on
xlabel('Frequency (Hz)')
ylabel('Magnitude (dB)')
