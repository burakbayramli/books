
%
%  m-file for Example 8B.3 (Program 8B.3, p567)
%	Program name: prog8b3.m
%
Fs=2000;										%	Sampling frequency
FN=Fs/2;
fc1=200/FN;
fc2=300/FN;
[b,a]=butter(4,[fc1, fc2]);			%	Create and digitize analogue filter.
[z,p,k]=butter(4, [fc1, fc2]);
subplot(2,1,1)								%	Plot magnitude freq. response
[H, f]=freqz(b, a, 512, Fs);			
plot(f, abs(H))
xlabel('Frequency (Hz)')
ylabel('Magnitude Response ')
subplot(2,1,2)								%	Plot pole-zero diagram
zplane(b, a)

