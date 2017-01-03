function [c,nsec,dsec] = tf2rpf(b,a);
% Synopsis: [c,nsec,dsec] = tf2rpf(b,a).
% Real partial fraction decomposition of b(z)/a(z). The polynomials
% are in negative powers of z. The poles are assumed distinct.
% Input parameters:
% a, b: the input polynomials
% Output parameters:
% c: the free polynomial; empty if deg(b) < deg(a)

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

nsec = []; dsec = []; [c,A,alpha] = tf2pf(b,a);
while (length(alpha) > 0),
  if (imag(alpha(1)) ~= 0),
    dsec = [dsec; [1,-2*real(alpha(1)),abs(alpha(1))^2]];
    nsec = [nsec; [2*real(A(1)),-2*real(A(1)*conj(alpha(1)))]];
    alpha(1:2) = []; A(1:2) = [];
  else,
    dsec = [dsec; [1,-alpha(1),0]]; nsec = [nsec; [real(A(1)),0]];
    alpha(1) = []; A(1) = [];
  end
end

