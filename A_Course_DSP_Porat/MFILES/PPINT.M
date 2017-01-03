function y = ppint(x,h,L);
% Synopsis: y = ppint(x,h,M).
% L-fold expansion and convolution, by polyphase decomposition.
% Input parameters:
% x: the input sequence
% h: the FIR filter coefficients
% L: the expansion factor.
% Output parameters:
% y: the output sequence.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

lh = length(h); lq = floor((lh-1)/L) + 1;
q = flipud(reshape([reshape(h,1,lh),zeros(1,lq*L-lh)],L,lq));
lx = length(x); ly = lx*L+lh-1;
lv =lx + lq; % length of decimated sequences
v = zeros(L,lv);
for l = 1:L, v(l,1:lv-1) = conv(x,q(l,:)); end
y = reshape(flipud(v),1,L*lv);
y = y(1:ly);

