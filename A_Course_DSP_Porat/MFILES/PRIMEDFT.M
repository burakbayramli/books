function y = primedft(x,W);
% Synopsis: y =primedft(x,W).
% Brute-force DFT or conjugate DFT in case of prime length.
% Input parameters:
% x: the input vector, assumed to be a row
% W: vector of powers of W, assumed to be a row.
% Output:
% y: the output row vector.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N = length(x); n = 1:N-1; y = zeros(1,N); y(1) = sum(x);
for k = 1:N-1,
   y(k+1) = x(1) + sum(x(2:N).*W(rem(k*n,N)+1)); end

