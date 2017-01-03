function y = pvoc(x, r, n)
% y = pvoc(x, r, n)  Time-scale a signal to r times faster with phase vocoder
%      x is an input sound. n is the FFT size, defaults to 1024.  
%      Calculate the 25%-overlapped STFT, squeeze it by a factor of r, 
%      inverse spegram.
% 2000-12-05, 2002-02-13 dpwe@ee.columbia.edu.  Uses pvsample, stft, istft
% $Header: /homes/dpwe/public_html/resources/matlab/RCS/pvoc.m,v 1.2 2002/02/13 16:14:54 dpwe Exp $

if nargin < 3
  n = 1024;
end

% With hann windowing on both input and output, 
% we need 25% window overlap for smooth reconstruction
hop = n/4;
% Effect of hanns at both ends is a cumulated cos^2 window (for
% r = 1 anyway); need to scale magnitudes by 2/3 for
% identity input/output
scf = 2/3;

% Calculate the basic STFT, magnitude scaled
X = scf * stft(x', n, n, hop);

% Calculate the new timebase samples
[rows, cols] = size(X);
t = 0:r:(cols-2);
% Have to stay two cols off end because (a) counting from zero, and 
% (b) need col n AND col n+1 to interpolate

% Generate the new spectrogram
X2 = pvsample(X, t, hop);

% Invert to a waveform
y = istft(X2, n, n, hop)';
