function [d,c]=ch_analog_weights(band,L,dB,w1,w2)
% [d,c]=ch_analog_weights(band,L,w1,w2)
% 
% Chebyshev analog lowpass, highpass, bandpass, or bandstop weights.
%
% Arrays d and c are numerator and denominator weights
% of the analog filter in cascade form using single-pole sections 
% H(1,s),H(2,s),..., and H(L/2,s). Thus d and c are L/2 x 2 arrays,
% and H(s)=H(1,s)H(1,s)'...H(L/2,s)H(L/2,s)'.
% 
% Inputs: band =1(lowpass) 2(highpass) 3(bandpass) or 4(bandstop).
%         L    =# Lowpass poles. L must be even in this function.
%         dB   =stopband gain in dB; for example, "-60".
%         w1   =lower critical frequency in rad/s.
%         w2   =upper critical frequency (rad/s). Required only
%               if band = 2 or 3.
%
% Outputs: d =L/2 x 2 array of numerator weights.
%          c =L/2 x 2 array of denominator weights.
% Note: If band =3 or 4, L is doubled in the translation.
L2=L/2;
% Check for errors.
if 2*fix(L/2)~=L
    error('Number of lowpass poles (L) must be even.')
elseif(band<1 | band>4)
   error('"band" input must be 1, 2, 3, or 4.');
elseif(dB>-10)
   error('dB must be -10 or less.');
elseif(w1<=0)
   error('Frequency w1 must be in rad/s and >0');
elseif(band>2)
   if(nargin<5)
      error('5 arguments are required if band is >2.');
   elseif(w2<=w1)
      error('Frequency w2 must be in rad/s and >w1');
   end
end
% Define wc.
if(band<=2),
   wc=w1;
else
   wc=w2-w1;
end
% Compute and test ws.
ws=wc*cosh(acosh(sqrt(10^(-dB/10)-1))/L);
if(ws<=wc)
   error('Design won''t work. Please increase either L or dB.');
end
% Poles (column).
zeta=1/cosh(L*acosh(ws/wc));
alpha=(1/L)*asinh(1/zeta);
beta=(2*(1:L/2)-L-1)*pi/(2*L);
sigma=wc*(sinh(alpha)*cos(beta)+j*cosh(alpha)*sin(beta));
p=(-wc*ws./sigma)';
% Zeros (column).
sigma=j*wc*cos((2*(1:L/2)-1)*pi/(2*L));
z=(-wc*ws./sigma)';
% Lowpass filter.
if(band==1)
   d(:,1)=p;
   d(:,2)=-p.*z;
   c(:,1)=z;
   c(:,2)=-z.*p;
% Highpass filter.
elseif(band==2)
   d=[ones(L2,1) -wc^2./z];
   c=[ones(L2,1) -wc^2./p];
% Bandpass filter.
elseif(band==3)
   rz=(z+j*sqrt(4*w1*w2-z.^2))/2;
   d=[ones(L2,1) -rz].*(p*[1 1]);
   rz=(conj(z)+j*sqrt(4*w1*w2-conj(z).^2))/2;
   d(L2+1:2*L2,1:2)=[ones(L2,1) -rz];
   rp=(p+j*sqrt(4*w1*w2-p.^2))/2;
   c=[ones(L2,1) -rp].*(z*[1 1]);
   rp=(conj(p)+j*sqrt(4*w1*w2-conj(p).^2))/2;
   c(L2+1:2*L2,1:2)=[ones(L2,1) -rp];
% Bandstop filter.
elseif(band==4)
   rz=(wc^2./z+j*sqrt(4*w1*w2-wc^4./z.^2))/2;
   d=[ones(L2,1) -rz];
   rz=(wc^2./conj(z)+j*sqrt(4*w1*w2-wc^4./conj(z).^2))/2;
   d(L2+1:2*L2,1:2)=[ones(L2,1) -rz];
   rp=(wc^2./p+j*sqrt(4*w1*w2-wc^4./p.^2))/2;
   c=[ones(L2,1) -rp];
   rp=(wc^2./conj(p)+j*sqrt(4*w1*w2-wc^4./conj(p).^2))/2;
   c(L2+1:2*L2,1:2)=[ones(L2,1) -rp];
end
