% this routine takes an array f and returns its spectral derivative.  it
% wants f to be periodic of length 2^n where f(1) = f(2^n+1)  And so 
% x(2^n) = 2*pi - dx, for example.  It assumes f is real-valued.
%
% to call it: >> fd = fdiff(f);
%
% where you've already defined f
%
function [fd] = fdiff(f);

N = length(f);

v = fft(f);

clear i
v(1) = 0;
for k = 1:(N/2-1)
% k is the wave-number.  Note that the zero mode v(1) is untouched since
% it is the mean.
  v(1+k) = i*k*v(1+k);
  v(N-k+1) = -i*k*v(N-k+1);
end

% take the inverse transform.  I've cut off the imaginary parts since
% they're going to be at the level of round-off anyway.  (the true
% solution started out real-valued and will remain real.  If I wanted to
% be super-cautious, I'd keep the imaginary parts of the computed
% solution around to make sure they aren't growing on me.)
fd = real(ifft(v)); 





