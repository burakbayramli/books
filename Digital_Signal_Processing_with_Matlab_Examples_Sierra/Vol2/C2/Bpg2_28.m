% CDF 9/7 frequency response of filters

%coefficients
ah0=[-0.045635881557,-0.028771763114,0.295635881557,0.557543526229...
,0.295635881557,-0.028771763114,-0.045635881557];

sh0=[0.026748757411,-0.016864118443,-0.078223266529,0.266864118443...
,0.602949018236,0.266864118443,-0.078223266529,-0.016864118443,0.026748757411];

ah0n=(ah0*sqrt(2))/sum(ah0); %normalization
sh0n=(sh0*sqrt(2))/sum(sh0); %normalization

aH0=fft(ah0n,512); %frequency response
sH0=fft(sh0n,512); %frequency response


%the ah1(n) coefficients
ah1=fliplr(sh0n); ah1(1:2:end)=-ah1(1:2:end);
%the sh1(n) coefficients
sh1=fliplr(ah0n); sh1(1:2:end)=-sh1(1:2:end);

aH1=fft(ah1,512); %frequency response
sH1=fft(sh1,512); %frequency response

%display
w=0:(2*pi/511):pi;

subplot(2,2,1)
plot(w,abs(aH0(1:256)),'k');
axis([0 pi 0 2]);
title('|aH0(w)|');xlabel('w');

subplot(2,2,2)
plot(w,abs(aH1(1:256)),'k');
axis([0 pi 0 2]);
title('|aH1(w)|');xlabel('w');

subplot(2,2,3)
plot(w,abs(sH0(1:256)),'k');
axis([0 pi 0 2]);
title('|sH0(w)|');xlabel('w');

subplot(2,2,4)
plot(w,abs(sH1(1:256)),'k');
axis([0 pi 0 2]);
title('|sH1(w)|');xlabel('w');

