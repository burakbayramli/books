function [d,c]=analog_translate(band,dl,cl,w1,w2)
% [d,c]=analog_translate(band,dl,cl,w1,w2)
% 
% Translates analog lowpass to highpass, bandpass, or bandstop.
%
% Arrays [d,c] and [dl,cl] are numerator and denominator weights
% of analog filters in cascade form using single-pole sections: 
% H(1,s),H(2,s),..., and H(L/2,s). Thus d and c are L/2 x 2
% arrays, and H(s)=H(1,s)H(1,s)'...H(L/2,s)H(L/2,s)'.
% 
% Inputs: band =2(highpass), 3(bandpass), or 4(bandstop)
%         dl   =lowpass numerator weights.
%               dl(:,1)=all zeros for Butterworth filter.
%         cl   =lowpass denominator weights, all nonzero.
%         w1   =lower critical frequency in rad/s.
%         w2   =upper critical frequency (rad/s).  Required only
%               if band = 2 or 3.
%
% Outputs: d =L/2 x 2 array of numerator weights.
%          c =L/2 x 2 array of denominator weights.
% Note: If band =3 or 4, L is doubled in the translation.
narg=nargin;
[L2,nd]=size(dl);
[m,nc]=size(cl);
% Check for errors.
if(band<2 | band>4)
   error('"band" input must be 2, 3, or 4.');
elseif(w1<=0)
   error('Frequency w1 must be in rad/s and >0');
elseif(band>2)
   if(narg<5)
      error('5 arguments are required if band is >2.');
   elseif(w2<=w1)
      error('Frequency w2 must be in rad/s and >w1');
   end
elseif(L2~=m | nd~=2 | nc~=2),
   error('dl and cl must both be L/2 x 2 arrays.');
end
wc=w2-w1;
% Highpass
if(band==2)
   d=[dl(:,2) dl(:,1)*w1^2];
   c=[cl(:,2) cl(:,1)*w1^2];
% Bandpass
elseif(band==3)
   if(max(dl(:,1))==0),
      d=[dl(:,2) zeros(L2,1)];
      d(L2+1:2*L2,1:2)=[zeros(L2,1) ones(L2,1)];
   else
      root=sqrt(dl(:,2).^2-4*dl(:,1).^2*w1*w2);
      r1=(-dl(:,2)+root)./(2*dl(:,1));
      r2=(-dl(:,2)-root)./(2*dl(:,1));
      d(:,1)=dl(:,1);
      d(:,2)=-dl(:,1).*r1;
      d(L2+1:2*L2,1)=ones(L2,1);
      d(L2+1:2*L2,2)=-r2;
   end
   root=sqrt(cl(:,2).^2-4*cl(:,1).^2*w1*w2);
   r1=(-cl(:,2)+root)./(2*cl(:,1));
   r2=(-cl(:,2)-root)./(2*cl(:,1));
   c(:,1)=cl(:,1);
   c(:,2)=-cl(:,1).*r1;
   c(L2+1:2*L2,1)=ones(L2,1);
   c(L2+1:2*L2,2)=-r2;
% Bandstop
elseif(band==4)
   root=sqrt(dl(:,1).^2*wc^4-4*dl(:,2).^2*w1*w2);
   r1=(-dl(:,1)*wc^2+root)./(2*dl(:,2));
   r2=(-dl(:,1)*wc^2-root)./(2*dl(:,2));
   d(:,1)=dl(1:L2,2);
   d(:,2)=-dl(1:L2,2).*r1;
   d(L2+1:2*L2,1)=ones(L2,1);
   d(L2+1:2*L2,2)=-r2;
   root=sqrt(cl(:,1).^2*wc^4-4*cl(:,2).^2*w1*w2);
   r1=(-cl(:,1)*wc^2+root)./(2*cl(:,2));
   r2=(-cl(:,1)*wc^2-root)./(2*cl(:,2));
   c(:,1)=cl(1:L2,2);
   c(:,2)=-cl(1:L2,2).*r1;
   c(L2+1:2*L2,1)=ones(L2,1);
   c(L2+1:2*L2,2)=-r2;
end
