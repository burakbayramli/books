% Signal analysis by continuous wavelet transform
% Morlet Wavelet
% Heart sound: normal

% the signal
[yin,fin]=wavread('heart1.wav'); %read wav file
ndc=5; %decimation value
yo=yin(1:ndc:end); %signal decimation
fs=fin/ndc;

wy=2*pi*fs; %signal frequency in rad/s
Ts=1/fs; %time interval between samples;

% plot preparation
L=length(yo);
to=0:Ts:((L-1)*Ts);

%extract signal segment-----------------------------
ti=0; %initial time of signal segment (sec)
duy=1.5; %signal segment duration (sec)
tsg=ti:Ts:(duy+ti); %time intervals set
Ni=1+(ti*fs); %number of the initial sample
ND=length(tsg); %how many samples in signal segment
y=yo(Ni:(Ni+ND-1)); %the signal segment

%CWT algorithm--------------------------------------
CC=zeros(25,ND);
% CWT
nn=1:ND;
for ee=1:25,   
   s=ee*0.0005; %scales
   for rr=1:ND, %delays
      a=Ts*(rr-1);
      val=0;     
      %vectorized part
         t=Ts*(nn-1);
         x=(t-a)/s; %plug coeffs.
         psi=(1/sqrt(s))*(exp(-(x.^2)/2).*cos(5*x)); %wavelet
         for j=1:ND,
            val=val+(y(j).*psi(j));
         end;   
      CC(ee,rr)=val;
   end;
 end;

%display----------------------------------------------
figure (1)
subplot(2,1,1)
plot(tsg,y,'k');
axis([ti ti+duy min(y)-0.1 max(y)+0.1]);
xlabel('sec'); ylabel('signal');
title('Heart sound signal, a segment');
subplot(2,1,2)
imagesc(20*log(abs(CC)),[30 110]);
colormap('jet'); 
title('wavelet analysis')
xlabel('samples'); ylabel('scales');

%sound
soundsc(y,fs);

