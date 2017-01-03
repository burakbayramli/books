function X = edufft(x,swtch);
% Synopsis: X = edufft(x,swtch).
% An "educational" FFT algorithm, based on Cooley-Tukey frequency
% decimation procedure. Runs slow due to MATLAB recursion overhead.
% Input parameters:
% x: the input vector
% swtch: 0 for FFT, 1 for IFFT.
% Output:
% X: the output vector.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

N = length(x); x = reshape(x,1,N);
if (swtch), W = exp((j*2*pi/N)*(0:N-1));
else, W = exp((-j*2*pi/N)*(0:N-1)); end
X = ctrecur(x,W); if (swtch), X = (1/N)*X; end

