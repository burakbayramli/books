% this takes a vector u and returns k and amp.  k is the vector of
% wave-numbers, amp is the logarithm (base 10) of the fourier
% amplitudes.  If u comes from sampling a function with a uniform mesh,
% the the function is resolved if the highest amplitudes are at the
% level of round-off error.  (amp between -14 and -16).  
%
% [k,amp] = find_spec(u)
%
% If u is coming from a periodic function, the code wants u_0, u_1, ...
% u_{n-1} where u_0 = u_n.  That is u comes from x = 0:dx:2pi-dx, for
% example.
function [k,amp] = find_spec(u)

N = length(u);

v = fft(u);
% normalize v so that we get something that reflects the analytical inner
% product.
v = v/N;

% kk = 0
kk = 0;
amp(kk+1) = log(abs(v(kk+1))+10e-16);
k(kk+1) = kk;
for kk=1:floor(N/2)-1
  amp(kk+1) = log10(sqrt(v(kk+1)*v(N-kk+1)) + 1e-16);    
  k(kk+1) = kk;
end
