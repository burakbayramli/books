% Example of biorthogonal filter bank
% H0 and H1 are not power complementary

h0=[1 2 1]; %the filter H0
h0=(1/4)*h0; %scaling
fx=conv([1 2 1],[-1 4 -1]); 
fx=(1/8)*fx; %scaling
f0=2*fx; %the filter F0
for n=1:5, h1(n)=((-1)^(n-1))*fx(n); end; % the filter H1

w=0:(2*pi/511):pi;
H0=abs(fft(h0,512)); %discrete Fourier transform
H1=abs(fft(h1,512)); % "  "  "
PH0=H0.*H0;
PH1=H1.*H1;
PHT=PH0+PH1;
plot(w,PH0(1:256),'kx'); hold on;
plot(w,PH1(1:256),'k');
plot(w,PHT(1:256),'b');
axis([0 pi 0 1.5]);
title('Prototype filter and the biorthogonal: frequency response of power');
xlabel('w');
