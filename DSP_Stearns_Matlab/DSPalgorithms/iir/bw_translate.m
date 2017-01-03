function [z,p,coef]=bw_translate(band,pl,v1)
% [z,p]=bw_translate(band,pl,v1)
% 
% Translates Butterworth analog lowpass poles.
%
% Inputs: band =2(highpass), 3(bandpass), or 4(bandstop)
%         pl   =vector of lowpass poles in 2nd quadrant of s-plane.
%         vl   =(not needed for highpass).
%              =frequency (rad/s) just >wc (bandpass and bandstop).
%               (Note: In the Butterworth filter, the cutoff freq.,
%                wc rad/s, equals magnitude of any element of pl.)
% Outputs: z=vector of L/2 or L zeros on real axis of s-plane.
%          p=vector of L/2 or L poles in 2nd quadrant of s-plane.
%          coef=c such that max(c*|H(jw)|)=1 after translation.
pl=row_vec(pl);
L2=length(pl);
narg=nargin;
wc=abs(pl(1));
if(narg<3)
   v1=atan(wc)/pi+.05*(.5-atan(wc)/pi);
end
w1=tan(pi*v1);
% Check for errors.
if(band<2 | band>4)
   error('"band" input must be 2, 3, or 4.');
elseif(min(abs(pl))==0),
   error('Cannot translate a lowpass pole at s=0.')
elseif(narg<2)
   error('At least 2 arguments are required.');
elseif(band>2 & (v1<=atan(wc)/pi | v1>=.5))
   error('Frequency v1 must be in the range (vc,0.5) Hz-s');
end
% Highpass
if(band==2)
   z=zeros(1,L2);
   p=pl;
   coef=1;
% Bandpass
elseif(band==3)
   z=zeros(1,2*L2);
   w2=w1+wc;
   p(1:L2)=(pl+sqrt(pl.^2-4*w1*w2))/2;
   p(L2+1:2*L2)=(pl-sqrt(pl.^2-4*w1*w2))/2;
   coef=wc^2;
% Bandstop
elseif(band==4)
   w2=w1+wc;
   b=wc^2*ones(1,L2)./pl;
   z=sqrt(-w1*w2)*[ones(1,L2) -ones(1,L2)];
   p(1:L2)=(b+sqrt(b.^2-4*w1*w2))/2;
   p(L2+1:2*L2)=(b-sqrt(b.^2-4*w1*w2))/2;
   coef=1;
end
z=real(z)+j*abs(imag(z));
p=real(p)+j*abs(imag(p));
