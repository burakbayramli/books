function [P,nsgmts,v]=pds2(x,y,N,windo,overlap)
% [P,nsgmts,v]=pds2(x,y,N,windo,overlap)
%
% Computes the cross power density spectrum of x and y in the frequency
% range from 0 to half the sampling rate.  See text, Chapter 7.
% 
% Inputs:
%  x,y     = Input signal vectors.
%  N       = segment length (even).  Number of spectral components = N/2.
%  windo   = Data window type:
%                1) Boxcar            4) Hanning
%                2) Tapered           5) Hamming
%                3) Triangular        6) Blackman
%  overlap = Fraction that each data segment of size N
%            overlaps its predecessor.  Must be greater than or equal 0
%            and less than 1.
% Outputs:
%  P       = Cross power density spectrum, P(1:N/2), P =first half of avg.
%            cross periodogram of x & y; hence sum(P) ~= (N/2)*mean(x.*y).
%  nsgmts  = Number of overlapping segments averaged together.
%  v       = Vector of frequencies in [0,.5). P(n) is at v(n) Hz-s.
%
% See also pds, pds1, gain, gain, gain_f

x=x(:);                                         %x,y =column vectors
y=y(:);
L=min(length(x),length(y));

if nargin <5,
    error('pds2 must have 5 arguments.');
elseif L<N,
   error('Length of x and/or y is < DFT size.');
elseif (N<8 | mod(N,2)==1),
   error('DFT size must be even and at least 8.');
end

P=zeros(N,1);                                   %P =column vector
nshift=min(N,max(1,round(N*(1-overlap))));
nsgmts=fix(1+(L-N)/nshift);
w=window(N,windo);
for isegmt=0:nsgmts-1
   xx=w'.*x((nshift*isegmt+1):(nshift*isegmt+N));
   yy=w'.*y((nshift*isegmt+1):(nshift*isegmt+N));
   P=P+conj(fft(xx)).*fft(yy);
end
% Note: division by w*w' includes division by N in 
%       accordance with text eq. 7.24.
P=P(1:N/2)/(nsgmts*w*w');                       %avg. normalized for widow
v=(0:N/2-1)'/N;                                  %v =column vector