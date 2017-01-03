function y = udftsynt(v,h,M);
% Synopsis: y = udftsynt(v,h,M).
% Maximally decimated uniform DFT synthesis filter bank.
% Input parameters:
% v: a matrix whose rows are the input sequences
% h: the FIR filter coefficients
% M: the expansion factor.
% Output parameters:
% y: the output sequence

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

lh = length(h); lq = floor((lh-1)/M) + 1;
q = flipud(reshape([reshape(h,1,lh),zeros(1,lq*M-lh)],M,lq));
v = fft(v);
y = [];
for m = 1:M, y = [conv(v(m,:),q(m,:)); y]; end
y = y(:).';

