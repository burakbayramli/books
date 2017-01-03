
%
%  m-file for Example 8B.5 (Program 8B.5, p571)
%	Program name: prog8b5.m
%
Fs=8000;										%	Sampling frequency
Ap=3;
As=20;
wp=2000/4000;
ws=500/4000;
[N, wc]=buttord(wp, ws, Ap, As);		% Determine filter order
[zz, pz, kz]=butter(N,2000/4000, 'high');	% Digitise filter
[b, a]=butter(N, 2000/4000, 'high');
subplot(2,1,1)								%	Plot magnitude freq. response
[H, f]=freqz(b, a, 512, Fs);			
plot(f, abs(H))
xlabel('Frequency (Hz)')
ylabel('Magnitude Response ')
subplot(2,1,2)								%	Plot pole-zero diagram
zplane(b, a)

