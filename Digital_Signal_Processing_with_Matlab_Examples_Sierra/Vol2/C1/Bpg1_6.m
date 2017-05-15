% Frequency response of the power of power symmetric filters
c=1/sqrt(2);
h0=[c c]; %low-pass filter
h1=[c -c]; %high-pass filter
w=0:(2*pi/511):pi;
H0=abs(fft(h0,512)); %discrete Fourier transform
H1=abs(fft(h1,512));  % "  "  "
PH0=H0.*H0;
PH1=H1.*H1;
PHT=PH0+PH1;
plot(w,PH0(1:256),'kx'); hold on;
plot(w,PH1(1:256),'k');
plot(w,PHT(1:256),'b');
axis([0 pi 0 2.5]);
title('Power symmetric filters: frequency response of power');
xlabel('w');
