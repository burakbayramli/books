function [b,a]=iir_lowpass_weights(type,NUc,NUs,dB)
% [b,a]=iir_lowpass_weights(type,NUc,NUs,dB)
%
% [b,a]=Weights of lowpass IIR filter in cascade form w/ L poles.
% Transfer function H(z)=product of L/2 transfer functions:
%            H(z)=H(1,z)H(2,z)...H(L/2,z).
% The numerator weights of H(k,z) are [b(k,1) b(k,2) b(k,3)].
% The denominator weights are [1 a(k,2) a(k,3)].
% Thus, a and b are (L/2 x 3) arrays of weights, with a(:,1)=ones.
% Inputs: type ='Butterworth' or 'Chebyshev' (1st letter checked).
%         NUc  =cutoff (-3dB) frequency in Hz-s.
%         NUs  =stopband frequency in Hz-s.
%         dB   =maximum stopband power gain in dB. Must be <-6.

% Use n_lowpass_poles, which checks for errors, to get L.
L=n_lowpass_poles(type,NUc,NUs,dB);
% Warp the critical frequencies.
wc=tan(pi*NUc);
ws=tan(pi*NUs);