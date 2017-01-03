
%
% m-file for Example 8B.6 (Program 8B.6, p572)
%	Program name: prog8b6.m
%	Band pass filter
%
Fs=1000;										%	Sampling frequency
Ap=3;
As=20;
Wp=[200/500, 300/500];					%	Band edge frequencies
Ws=[50/500, 450/500];
[N, Wc]=buttord(Wp, Ws, Ap, As);		% Determine filter order
[zz, pz, kz]=butter(N,Wp);				% Digitise filter
[b, a]=butter(N, Wp);
subplot(2,1,1)								%	Plot magnitude freq. response
[H, f]=freqz(b, a, 512, Fs);			
plot(f, abs(H))
xlabel('Frequency (Hz)')
ylabel('Magnitude Response ')
subplot(2,1,2)								%	Plot pole-zero diagram
zplane(b, a)

