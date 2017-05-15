% Signal analysis by continuous wavelet transform
% Morlet Wavelet
% Lung study: crackles

% the signal
[yin,fin]=wavread('crackles.wav'); %read wav file
yo=yin(:,1); %one of the 2 stereo channels
ndc=5; %decimation value
yo=yin(1:ndc:end); %signal decimation
fs=fin/ndc;

wy=2*pi*fs; %signal frequency in rad/s
Ts=1/fs; %time interval between samples;

% plot preparation
L=length(yo);
to=0:Ts:((L-1)*Ts);

%extract signal segment-----------------------------
ti=7.8; %initial time of signal segment (sec)
duy=1.6; %signal segment duration (sec)
tsg=ti:Ts:(duy+ti); %time intervals set
Ni=1+(ti*fs); %number of the initial sample
ND=length(tsg); %how many samples in signal segment
y=yo(Ni:(Ni+ND-1)); %the signal segment

%CWT algorithm--------------------------------------
CC=zeros(30,ND);
% CWT
nn=1:ND;
for ee=1:30,   
   s=ee*0.004; %scales
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
figure(1)
m=L/2; %only the first half of the signal (10 sec.)
plot(to(1:m),yo(1:m),'k')
title('Complete respiration signal')
xlabel('time');

figure (2)
subplot(2,1,1)
plot(tsg,y,'k');
axis([ti ti+duy min(y)-0.1 max(y)+0.1]);
xlabel('sec'); ylabel('signal');
title('Respiration signal, a segment');
subplot(2,1,2)
imagesc(CC);
colormap('jet'); 
title('wavelet analysis')
xlabel('samples'); ylabel('scales');

%sound
soundsc(y,fs);

