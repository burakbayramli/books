
%
%  m-file for Example 8B.4 (Program 8B.4, p569)
%	Program name: prog8b4.m
%
Fs=8000;										%	Sampling frequency
Ap=3;
As=20;
wp=500/4000;
ws=2000/4000;
[N, wc]=buttord(wp, ws, Ap, As);		% Determine filter order
[zz, pz, kz]=butter(N,500/4000);		% Digitise filter
[b, a]=butter(N, 500/4000);
subplot(2,1,1)								%	Plot magnitude freq. response
[H, f]=freqz(b, a, 512, Fs);			
plot(f, abs(H))
xlabel('Frequency (Hz)')
ylabel('Magnitude Response ')
subplot(2,1,2)								%	Plot pole-zero diagram
zplane(b, a)

