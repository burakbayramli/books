function u = udftanal(x,g,M);
% Synopsis: u = udftanal(x,g,M).
% Maximally decimated uniform DFT analysis filter bank.
% Input parameters:
% x: the input sequence
% g: the FIR filter coefficients
% M: the decimation factor.
% Output parameters:
% u: a matrix whose rows are the output sequences.

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

lg = length(g); lp = floor((lg-1)/M) + 1;
p = reshape([reshape(g,1,lg),zeros(1,lp*M-lg)],M,lp);
lx = length(x); lu = floor((lx+M-2)/M) + 1;
x = [zeros(1,M-1),reshape(x,1,lx),zeros(1,M*lu-lx-M+1)];
x = flipud(reshape(x,M,lu)); % the decimated sequences
u = [];
for m = 1:M, u = [u; conv(x(m,:),p(m,:))]; end
u = ifft(u);

