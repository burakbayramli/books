%
%  m-file for the design Example 8B.8 (Program 8B.8, p576)
%	Program Name: prog8b8.m
%
Ap=0.25;
As=45;
Fs=100000;
Wp=[20500/50000, 23500/50000];		%	Band edge frequencies
Ws=[19000/50000, 25000/50000];
[N,Wc]=ellipord(Wp, Ws, Ap, As);		%	Determine filter order
[b, a]=ellip(N, Ap, As, Wc);			%	Determine filter coeffs
[z, p, k]=ellip(N, Ap, As,Wc);		%	Determine poles and zeros
sos=zp2sos(z, p,k);						%	Convert to second order sections
subplot(2,1,1)								%	Plot magnitude freq. response
[H, f]=freqz(b, a, 512, Fs);			
plot(f, 20*log10(abs(H)))
xlabel('Frequency (Hz)')
ylabel('Magnitude Response (dB)')
subplot(2,1,2)								%	Plot pole-zero diagram
zplane(b, a)


