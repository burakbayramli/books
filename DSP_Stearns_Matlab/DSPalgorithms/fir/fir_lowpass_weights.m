function b=fir_lowpass_weights(N,vc)
% b=fir_lowpass_weights(N,nu)
%
% b   =unwindowed row vector of linear-phase lowpass FIR filter weights.
% N   =length of b (impulse response). (should be odd).
%     (If N is even, it is changed to N+1 with a warning.)
% vc =cutoff frequency in Hz-s.
% See also: fir_weights
L=fix(N/2);
% Check for errors.
if(2*L+1~=N),
   N=2*L+1;
   fprintf('Warning: # weights has been increased to %5.0f.\n',N);
end
if(vc>=.5 | vc<=0),
   error('Cutoff freq. vc must be > 0 and < 0.5 Hz-s.');
end
k=1:L;
b1=sin(2*pi*vc*k)./(pi*k);
b=[b1(L:-1:1) 2*vc b1(1:L)];