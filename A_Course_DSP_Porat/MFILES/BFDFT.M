function y = bfdft(x,swtch);
% Synopsis: y = bfdft(x,swtch).
% Brute-force DFT or IDFT.
% Input parameters:
% x: the input vector
% swtch: 0 for DFT, 1 for IDFT.
% Output parameters:
% y: the output vector.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N = length(x);
x = reshape(x,1,N);
n = 0:N-1;
if (swtch), W = exp((j*2*pi/N)*n);
else, W = exp((-j*2*pi/N)*n); end
y = zeros(1,N);
for k = 0:N-1,
   y(k+1) = sum(x.*(W.^k));
end
if (swtch), y = (1/N)*y; end

