function [z,p]=ch_translate(band,zl,pl,vc,v1)
% [z,p]=ch_translate(band,zl,pl,wc,w1)
% 
% Translates Chebyshev analog lowpass poles and zeros.
%
% Inputs: band =2(highpass), 3(bandpass), or 4(bandstop)
%         zl   =vector of lowpass zeros above real axis of s-plane.
%         pl   =vector of lowpass poles in 2nd quadrant of s-plane.
%         vc   =lowpass cutoff frequency in Hz-s.
%         vl   =lower band frequency (Hz-s) if needed (see text).
% Outputs: z=vector of L/2 or L bandpass zeros on s-plane.
%          p=vector of L/2 or L bandpass poles in 2nd quadrant of s-plane.
pl=row_vec(pl);
zl=row_vec(zl);
L2=length(pl);
narg=nargin;
% Check for errors.
if(band<2 | band>4)
   error('"band" input must be 2, 3, or 4.');
elseif(length(zl)~=L2)
   error('Input zl and pl vectors must the the same length.');
elseif(min(abs(pl))==0),
   error('Cannot translate a lowpass pole at s=0.')
elseif(band>1 & narg<5)
   error('5 arguments are required if band is >1.');
elseif(vc<=0 | vc>=.5 | v1<=0 | v1>=.5)
   error('Frequencies vc and v1 must be in range (0,0.5) Hz-s');
end
% Warp the critical frequencies.
wc=tan(pi*vc);
w1=tan(pi*v1);
% Highpass
if(band==2)
   z=wc^2*ones(1,L2)./zl;
   p=wc^2*ones(1,L2)./pl;
% Bandpass
elseif(band==3)
   w2=w1+wc;
   z(1:L2)=(zl+sqrt(zl.^2-4*w1*w2))/2;
   z(L2+1:2*L2)=(zl-sqrt(zl.^2-4*w1*w2))/2;
   p(1:L2)=(pl+sqrt(pl.^2-4*w1*w2))/2;
   p(L2+1:2*L2)=(pl-sqrt(pl.^2-4*w1*w2))/2;
% Bandstop
elseif(band==4)
   w2=w1+wc;
   b=wc^2*ones(1,L2)./zl;
   z(1:L2)=(b+sqrt(b.^2-4*w1*w2))/2;
   z(L2+1:2*L2)=(b-sqrt(b.^2-4*w1*w2))/2;
   b=wc^2*ones(1,L2)./pl;
   p(1:L2)=(b+sqrt(b.^2-4*w1*w2))/2;
   p(L2+1:2*L2)=(b-sqrt(b.^2-4*w1*w2))/2;
end
p=real(p)+j*abs(imag(p));
z=real(z)+j*abs(imag(z));
