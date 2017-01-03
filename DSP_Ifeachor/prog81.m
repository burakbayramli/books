%
% Program 8.1, p511. m-file for the analog filter design 
% (Example 8.19)
% program name: prog81.m
%
FN=1000/2;
fc=300;							% cut offf frequency
N=5;								% filter order
[z, p, k]=buttap(N);			% create an analog filter
w=linspace(0, FN/fc, 1000);	% plot the response of filter
h=freqs(k*poly(z), poly(p), w);
f=fc*w;
plot(f, 20*log10(abs(h))), grid
ylabel('Magnitude (dB)')
xlabel('Frequency (Hz)')
