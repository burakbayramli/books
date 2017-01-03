function y = ppdec(x,h,M);
% Synopsis: y = ppdec(x,h,M).
% Convolution and M-fold decimation, by polyphase decomposition.
% Input parameters:
% x: the input sequence
% h: the FIR filter coefficients
% M: the decimation factor.
% Output parameters:
% y: the output sequence.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

lh = length(h); lp = floor((lh-1)/M) + 1;
p = reshape([reshape(h,1,lh),zeros(1,lp*M-lh)],M,lp);
lx = length(x); ly = floor((lx+lh-2)/M) + 1;
lu = floor((lx+M-2)/M) + 1; % length of decimated sequences
u = [zeros(1,M-1),reshape(x,1,lx),zeros(1,M*lu-lx-M+1)];
u = flipud(reshape(u,M,lu)); % the decimated sequences
y = zeros(1,lu+lp-1);
for m = 1:M, y = y + conv(u(m,:),p(m,:)); end
y = y(1:ly);

