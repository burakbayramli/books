% Example of orthogonal filter bank
% H0 and H1 are power complementary
r=2-sqrt(3);
h0=conv([1 2 1],[r -1]); 
sc=4*(1-sqrt(3)); %scaling with sum(h0(i))
h0=(1/sc)*h0; %prototype filter H0
for n=1:4, h1(n)=((-1)^n)*h0(5-n); end; % the CQF H1

w=0:(2*pi/511):pi;
H0=abs(fft(h0,512)); %discrete Fourier transform
H1=abs(fft(h1,512)); % "  "  "
PH0=H0.*H0;
PH1=H1.*H1;
PHT=PH0+PH1;
plot(w,PH0(1:256),'kx'); hold on;
plot(w,PH1(1:256),'k');
plot(w,PHT(1:256),'b');
axis([0 pi 0 1.2]);
title('Prototype filter and the CQF: frequency response of power');
xlabel('w');
