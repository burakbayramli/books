function [b,a]=ch_weights(band,L,dB,v1,v2)
% [b,a]=ch_weights(band,L,dB,v1,v2)
%
% [b,a] = weights of a digital Chebyshev filter in cascade form.
% Inputs:
%   band =1(lowpass), 2(highpass), 3(bandpass), or 4(bandstop).
%   L    =number of lowpass poles. L is doubled for bands 3 & 4.
%         L must be an even integer.
%   dB   =maximum stopband gain in dB; for example, "-60".
%   v1   =first critical frequency (Hz-s); 0.0<v1<0.5.
%   v2   =second critical frequency; v1<v2<0.5.
%         v2 is used only if band=3 or band=4.
% Outputs:
%   b =numerator weights of cascade H(z) with quadratic sections.
%     # sections =L/2 for bands 1&2, or L for bands 3&4.
%     Thus size(b)=[L/2,3] for band = 1&2, or [L,3] for band = 3&4.
%   a =denominator weights, dimensioned the same as b.

% Check for errors;
if(length(band(:))>1 | length(L(:))>1 |...
   length(v1(:))>1 | (nargin==5 & length(v2(:))>1)),
   error('All arguments must be scalars.');
elseif(v1<=0 | v1>=.5)
   error('Argument v1 must be >0.0 and <0.5')
elseif(nargin==5 & (v2<=v1 | v2>=0.5))
   error('Argument v2 must be >v1 and <0.5')
elseif(nargin==5 & v1<=0)
   error('Argument v1 must be >0 and <v2')
end

% Get the anlalog weights and convert to digital.
if(nargin<5)
   [d,c]=ch_analog_weights(band,L,dB,tan(pi*v1));
else
   [d,c]=ch_analog_weights(band,L,dB,tan(pi*v1),tan(pi*v2));
end
[b,a]=bilin(d,c);