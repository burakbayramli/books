function [d,c]=bw_analog_weights(band,L,w1,w2)
% [d,c]=bw_analog_weights(band,L,w1,w2)
% 
% Butterworth analog lowpass, highpass, bandpass, or bandstop weights.
%
% Arrays d and c are numerator and denominator weights
% of the analog filter in cascade form using single-pole sections 
% H(1,s),H(2,s),..., and H(L/2,s). Thus d and c are L/2 x 2 arrays,
% and H(s)=H(1,s)H(1,s)'...H(L/2,s)H(L/2,s)'.
% 
% Inputs: band =1(lowpass) 2(highpass) 3(bandpass) or 4(bandstop)
%         L    =# Lowpass poles.  Must be even.
%         w1   =lower critical frequency in rad/s.
%         w2   =upper critical frequency (rad/s). Required only
%               if band = 2 or 3.
%
% Outputs: d =L/2 x 2 array of numerator weights.
%          c =L/2 x 2 array of denominator weights.
% Note: If band=1, c(:,1)=1 and c(:,2)= -(analog pole)
 %      If band=3 or 4, L is doubled in the translation.
L2=L/2;
% Check for errors.
if(mod(L,2)==1),
   error('Number of lowpass poles (L) must be even.')
end
if(band<1 | band>4)
   error('"band" input must be 1, 2, 3, or 4.');
elseif(w1<=0)
   error('Frequency w1 must be in rad/s and >0');
elseif(band>2)
   if(nargin<4)
      error('4 arguments are required if band is >2.');
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
% Lowpass poles:
p=wc*exp(j*(2*[1:L2]+L-1)*pi/(2*L))';
% Lowpass filter - to be used or translated.
   d(:,1)=zeros(L2,1);
   d(:,2)=wc*ones(L2,1);
   c(:,1)=ones(L2,1);
   c(:,2)=-p;
% Highpass filter.
if(band==2)
   d=[d(:,2) zeros(L2,1)];
   c=[c(:,2) c(:,1)*wc^2];
% Bandpass filter.
elseif(band==3)
   d=[d(:,2) zeros(L2,1)];
   d(L2+1:2*L2,1:2)=[zeros(L2,1) ones(L2,1)];
   root=sqrt(c(:,2).^2-4*c(:,1).^2*w1*w2);
   r1=(-c(:,2)+root)./(2*c(:,1));
   r2=(-c(:,2)-root)./(2*c(:,1));
   c(:,1)=c(:,1);
   c(:,2)=-c(:,1).*r1;
   c(L2+1:2*L2,1)=ones(L2,1);
   c(L2+1:2*L2,2)=-r2;
% Bandstop
elseif(band==4)
   root=sqrt(d(:,1).^2*wc^4-4*d(:,2).^2*w1*w2);
   r1=(-d(:,1)*wc^2+root)./(2*d(:,2));
   r2=(-d(:,1)*wc^2-root)./(2*d(:,2));
   d(:,1)=d(1:L2,2);
   d(:,2)=-d(1:L2,2).*r1;
   d(L2+1:2*L2,1)=ones(L2,1);
   d(L2+1:2*L2,2)=-r2;
   root=sqrt(c(:,1).^2*wc^4-4*c(:,2).^2*w1*w2);
   r1=(-c(:,1)*wc^2+root)./(2*c(:,2));
   r2=(-c(:,1)*wc^2-root)./(2*c(:,2));
   c(:,1)=c(1:L2,2);
   c(:,2)=-c(1:L2,2).*r1;
   c(L2+1:2*L2,1)=ones(L2,1);
   c(L2+1:2*L2,2)=-r2;
end
