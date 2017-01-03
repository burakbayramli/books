function L=ch_lowpass_size(vc,vs,dB)
% L=ch_lowpass_size(vc,vs,dB)
%
% L   =# lowpass Chebyshev poles necessary to meet specs.
%      (NOTE: L is even so poles will be in conjugate pairs.)
% vc  =cutoff frequency of lowpass digital filter in Hz-s.
% vs  =frequency at beginning of stopband in Hz-s.
% dB  =maximum stopband power gain in dB. Must be <-6.
% See also: bw_lowpass_size

% Check for errors.
if(nargin~=3)
   error('iir_lowpass_ch_size requires 3 input arguments.');
elseif(min(vc,vs)<=0 | max(vc,vs)>=.5)
   error('Arguments vc and vs must be in within (0,0.5).');
elseif(dB>=-6)
   error('Stopband power gain spec. must be < -6 dB.');
end
% Ratio of warped frequencies.
r=tan(pi*vs)/tan(pi*vc);
% Chebyshev size
L=(6.02-dB)/(8.69*acosh(r));
% Make L an even integer.
L=2*ceil(L/2);