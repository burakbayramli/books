%
%	Program name - prog7b5.m, p451
%  m-file for computing the coefficients of an FIR frequency sampling
%  filter 
%
Fs=2000;										%	Sampling frequency
N=15;											%	Filter length
fd=[0 1/7 2/7 3/7 4/7 5/7 6/7 1];	%	Frequency sampling points
Hd=[1 1 1 1 0.5571 0.0841 0 0];		%	Frequency samples
hn=fir2(N-1, fd, Hd);					%	Compute the impulse response coeffs.
[H, f] = freqz(hn, 1, 512, Fs);		%	Plot the magnitude frequency response
plot(f, abs(H)), grid on
xlabel('Frequency (Hz)')
ylabel('Magnitude ')
