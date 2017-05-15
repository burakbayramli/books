%Continuous wavelet analysis with Mexican hat
%harp signal

[y1,fs1]=wavread('doorbell1.wav'); %read wav file
y1=y1(1:16000); %select part of the signal
Nss=length(y1);
soundsc(y1,fs1); %hear wav

t=(-Nss/2):((Nss/2)-1); %normalized time intervals set
C=2/(sqrt(3)*sqrt(sqrt(pi))); %Mexican hat constant

NS=128; %number of scales
CC=zeros(NS,Nss); %space for wavelet coeffs.

for ee=1:NS,
   s=(ee*1); %scales
   %the scaled Mexican hat 
   ts=t/s;
   psi=C*(1-(ts.^2).*exp(-0.5*ts.^2));
   %CWT
   CC(ee,:)=abs(ifft(fft(psi).*fft(y1')));
end

figure(1)
imagesc(CC);   
title('Scalogram of doorbell');
xlabel('samples'); ylabel('scales');

