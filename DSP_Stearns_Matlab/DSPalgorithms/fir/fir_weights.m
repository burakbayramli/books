function b=fir_weights(N,band,windo,v1,v2)
% b=fir_weights(N,band,windo,v1,v2)
%
% b    =Linear-phase FIR filter windowed weights as a row vector.
% N    =Length of b. N must be an odd number.
%       (If N is even, it is changed to N+1 with a warning.)
% band =1 (lowpass), 2 (highpass), 3(bandpass), or 4 (bandstop).
% windo=1 (boxcar),  2 (tapered),  3(tent)  4 (hanning),
%       5 (hamming), 6(blackman), or 7(kaiser) 
%        (In the last case windo is a vector=[7,beta], with beta
%         in range [4.0,9.0]. See Eq. 5.8 in the text.)
% v1   =cutoff or lower band edge in Hz-s.
% v2   =upper band edge in Hz-s. Needed for bandpass & bandstop only.
% See also: fir_lowpass_weights
L=fix(N/2); beta=0; vm=[0 .5 0 0];
% Check for errors.
if(2*L+1~=N),
   N=2*L+1
   fprintf('Warning: # weights has been increased to %5.0f.\n',N);
end
if(band==3 | band==4),
   if(nargin<5)
      error('fir_weights: 2 must be specified for "band" filters.');
   else
      if(v2<=v1|v2>.5),
         error('fir_weights: v2 must be > v1 and < 0.5.');
      end
   end
end
if(v1<0|v1>0.5),   
    error('fir_weights: v1 must be >= 0 and <= 0.5.');
elseif(windo(1)<1 | windo(1)>7)
    error('Third argument (windo(1)) must be in the range [1,7]');
end
if(windo(1)==7)
    if(length(windo)~=2)
        error('Beta is not properly specified for Kaiser window.');
    end
    beta=windo(2);
end

% Allpass and lowpass weights.
k=[0:L-1];
bAP=[zeros(1,L),1,zeros(1,L)];
bL=sin(2*pi*v1*(k-L))./(pi*(k-L));
bLP1=[bL,2*v1,bL(L:-1:1)];

% Conversion to one of 4 filter bands.
if(band==1),
   b=bLP1;
elseif(band==2),
   b=bAP-bLP1;                          % Eq. 5.12(1)
elseif(band==3 | band==4),
   b2=sin(2*pi*v2*(k-L))./(pi*(k-L));
   bLP2=[b2,2*v2,b2(L:-1:1)];
   if(band==3),
      b=bLP2-bLP1;                       % Eq. 5.12(2)
      vm(3)=(v1+v2)/2;                   % Middle of passband.
   elseif(band==4),
      b=bAP-(bLP2-bLP1);                 % Eq. 5.12(3)
   end
else
   error('fir_weights: band must = 1, 2, 3, or 4');
end

% Apply the window to the weights, and force the max. gain to 1.
b=b.*window(N,windo(1),beta);
b=b/abs(gain_f(b,1,vm(band)));           % See p.121.