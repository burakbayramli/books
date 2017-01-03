function w=window(N,type,beta)
% w=window(N,type,beta)
%
% w    =N-point data widow as a row vector.
%
% N    =length of window.
% type =any of the following (only the first 4 letters matter,
%       or you may use the number):
%         1)'boxcar'   2)'tapered'   3)'tent'   4)'hanning'
%         5)'hamming'  6)'blackman'  7)'kaiser' 
% beta =parameter, usually from 4 to 9, used only with 'kaiser'.
% See also: bessel_0, fir_weights

if(N<4),
   error('Window length (N) must be at least 4.');
end
k=[0:N-1];
if(strncmp(type,'boxcar',4)==1 | ...
   strncmp(type,'Boxcar',4)==1 | type==1),
   w=ones(1,N);
elseif(strncmp(type,'tapered',4)==1 | ...
       strncmp(type,'Tapered',4)==1 | type==2),
   L=round((N-1)/10);
   w=ones(1,N);
   if(L>0),
      w(1:L)=.5*(1-cos([0:L-1]*pi/L));
      w(N-L+1:N)=.5*(1-cos([L-1:-1:0]*pi/L));
   end
elseif(strncmp(type,'tent',4)==1 | ...
       strncmp(type,'Tent',4)==1 | type==3),
   s=[0:fix(N/2)-1];
   if(mod(N,2)==0),
      w=[s rev(s)]/(N/2-1);
   else
      w=[s (N-1)/2 rev(s)]*2/(N-1);
   end
elseif(strncmp(type,'hanning',4)==1 | ...
       strncmp(type,'Hanning',4)==1 | type==4),
   w=0.5*(1-cos(2*pi*k/(N-1)));
elseif(strncmp(type,'hamming',4)==1 | ...
       strncmp(type,'Hamming',4)==1 | type==5),
   w=0.54-0.46*cos(2*pi*k/(N-1));
elseif(strncmp(type,'blackman',4)==1 | ...
       strncmp(type,'Blackman',4)==1 | type==6),
   w=0.42-0.5*cos(2*pi*k/(N-1))+0.08*cos(4*pi*k/(N-1));
elseif(strncmp(type,'kaiser',4)==1 | ...
       strncmp(type,'Kaiser',4)==1 | type==7),
   if(nargin<3),
      error('window: beta must be specified for the Kaiser window.');
   end
   w=(bessel_0(beta*sqrt(1-(2*k/(N-1)-1).^2))/bessel_0(beta));
else
   error('Window type is not correctly designated.');
end
