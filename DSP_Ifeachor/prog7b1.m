
%
%	prog7b1.m, p442			Program name
%
fc=0.53;							% cutoff frequency (normalized to Fs/2)
N= 10;							% Filter length (number of taps)
hd=fir1(N-1,fc,boxcar(N));	% Calculate truncated ideal impulse response coeffs
wn=hamming(N);					% Calculate Hamming window coefficients
hn=fir1(N-1,fc,wn);			% Obtain windowed coefficients
