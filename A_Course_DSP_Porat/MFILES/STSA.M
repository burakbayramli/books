function X = stsa(x,N,K,L,w,opt,M,theta0,dtheta);
% Synopsis: X = stsa(x,N,K,L,w,opt,M,theta0,dtheta).
% Short-time spectral analysis.
% Input parameters:
% x: the input vector
% N: segment length
% K: number of overlapping points in adjacent segments
% L: number of consecutive DFTs to average
% w: the window (a row vector of length N)
% opt: an optional parameter for nonstandard DFT:
%      'zp' for zero padding
%      'chirpf' for chirp Fourier transform
% M: length of DFT if zero padding or chirp was selected
% theta0, dtheta: parameters for chirp FT.
% Output:
% X: a matrix whose rows are the DFTs of the segments
%    (or averaged segments).

% Part of software package for the book:
% A Course in Digital Signal Processing
% by Boaz Porat, John Wiley & Sons, 1997

lx = length(x); nsec = ceil((lx-N)/(N-K)) + 1;
x = [reshape(x,1,lx), zeros(1,N+(nsec-1)*(N-K)-lx)];
nout = N; if (nargin > 5), nout = M; else, opt = 'n'; end
X = zeros(nsec,nout);
for n = 1:nsec,
   temp = w.*x((n-1)*(N-K)+1:(n-1)*(N-K)+N);
   if (opt(1) == 'z'), temp = [temp, zeros(1,M-N)]; end
   if (opt(1) == 'c'), temp = chirpf(temp,theta0,dtheta,M);
   else, temp = fftshift(fft(temp)); end
   X(n,:) = abs(temp).^2;
end
if (L > 1),
   nsecL = floor(nsec/L);
   for n = 1:nsecL, X(n,:) = mean(X((n-1)*L+1:n*L,:)); end
   if (nsec == nsecL*L+1), 
      X(nsecL+1,:) = X(nsecL*L+1,:); X = X(1:nsecL+1,:);
   elseif (nsec > nsecL*L),
      X(nsecL+1,:) = mean(X(nsecL*L+1:nsec,:));
      X = X(1:nsecL+1,:);
   else, X = X(1:nsecL,:); end
end

