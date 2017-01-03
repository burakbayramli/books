function [b,a]=bw_weights(band,L,v1,v2)
% [b,a]=bw_weights(band,L,v1,v2)
%
% [b,a] = weights of a digital Butterworth filter in cascade form.
% Inputs:
%   band =1(lowpass), 2(highpass), 3(bandpass), or 4(bandstop).
%   L    =number of lowpass poles. L is doubled for bands 3 & 4.
%         L must be even.
%   v1   =first critical frequency (Hz-s); 0.0<v1<0.5.
%   v2   =second critical frequency; v1<v2<0.5.
%         v2 is used only if band=3 or band=4.
% Outputs:
%   b =numerator weights of cascade H(z) with quadratic sections.
%     # sections =L/2 for bands 1&2, or L for bands 3&4.
%     Thus size(b)=[L/2,3] for bands 1&2, or [L,3] for bands 3&4.
%   a =denominator weights, dimensioned the same as b.
% See also: ch_weights

% Check for errors;
if(length(row_vec(band))>1 | length(row_vec(L))>1 |...
   length(row_vec(v1))>1 | (nargin==4 & length(row_vec(v2))>1)),
   error('All arguments must be scalars.');
elseif(v1<=0 | v1>=.5)
   error('Argument v1 must be >0.0 and <0.5')
elseif(nargin==4 & (v2<=v1 | v2>=0.5))
   error('Argument v2 must be >v1 and <0.5')
end
% Get the anlalog weights and convert to digital.
if(nargin<4)
   [d,c]=bw_analog_weights(band,L,tan(pi*v1));
else
   [d,c]=bw_analog_weights(band,L,tan(pi*v1),tan(pi*v2));
end
[b,a]=bilin(d,c);