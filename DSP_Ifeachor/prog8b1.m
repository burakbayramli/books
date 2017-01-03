
%
%  m-file for Example 8B.1 (Program 8B.1, p564).
%	Program name: prog8b1.m
%
N=2;											%	Filter order
Fs=1280;										%	Sampling frequency
fc=150;										%	Cutoff frequency
WC=2*pi*fc;									%	Cutoff frequency in radian
%
%	Create an analogue filter
%
[b,a]=butter(N,WC,'s');
[z,p,k]=butter(N, WC, 's');
%
%	Convert analogue filter into Discrete IIR filter
%
[bz, az]=impinvar(b, a, Fs);			%	Determine coeffs of IIR filter
subplot(2,1,1)								%	Plot magnitude freq. response
[H, f]=freqz(bz, az, 512, Fs);			
plot(f, 20*log10(abs(H)))
xlabel('Frequency (Hz)')
ylabel('Magnitude Response (dB)')
subplot(2,1,2)								%	Plot pole-zero diagram
zplane(bz, az)
zz=roots(bz);								%	Determine poles and zeros 
pz=roots(az);

