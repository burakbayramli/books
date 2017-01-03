function [dB,v]=power_gain(b,a,N)
% [dB,v]=power_gain(b,a,N)
%
% dB =N values of power gain in dB of a linear digital system.
%     A range of up to 200 dB is allowed.
% v  =N frequencies in Hz-s in the range [0,0.5] at which dB
%       values are computed.
% N  =# points (optional). If omitted, N=1024 is set internally.
% b  =numerator weights of linear system transfer function.
% a  =denominator weights.  a(1) must not be 0. Usually, a(1)=1.
%
% b and a can be arrays. If so, each row contains the weights of a
% section of the digital transfer function in cascade form.
%
% See also gain, gain_f, pds
if(nargin<3)
   N=1024;
end
% Use "gain" if possible. Use gain_f if N is too small for gain.
if N>=max(length(b(1,:)),length(a(1,:))),
    [H,v]=gain(b,a,N);
else
    v=linspace(0,.5,N)';
    H=gain_f(b,a,v);
end
Habs=abs(H);
H0=1.e-10*max(Habs);
dB=20*log10(max(Habs,H0));