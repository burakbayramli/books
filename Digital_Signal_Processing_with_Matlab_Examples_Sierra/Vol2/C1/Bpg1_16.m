% Example of biorthogonal filter bank
% the four filters, and a check of PR

h0=[1 2 1]; %the filter H0
h0=(1/4)*h0; %scaling
fx=conv([1 2 1],[-1 4 -1]); 
fx=(1/8)*fx; %scaling
f0=2*fx; %the filter F0
for n=1:5, h1(n)=((-1)^(n-1))*fx(n); end; % the filter H1
for n=1:3, f1(n)=-2*((-1)^(n-1))*h0(n); end; % the filter F1

% Check of PR
prod1=conv(f0,h0);
prod2=conv(f1,h1);
nodist=prod1+prod2

w=0:(2*pi/511):pi;
H0=abs(fft(h0,512)); %discrete Fourier transform
H1=abs(fft(h1,512)); % "  "  "
F0=abs(fft(f0,512)); % "  "  "
F1=abs(fft(f1,512)); % "  "  "


subplot(2,2,1)
plot(w,H0(1:256),'k');
axis([0 pi 0 1.2]);
ylabel('H0');xlabel('w');
title('frequency response (magnitude) of the four filters');
subplot(2,2,2)
plot(w,H1(1:256),'k');
axis([0 pi 0 1.2]);
ylabel('H1');xlabel('w');
subplot(2,2,3)
plot(w,F0(1:256),'k');
axis([0 pi 0 2.4]);
ylabel('F0');xlabel('w');
subplot(2,2,4)
plot(w,F1(1:256),'k');
axis([0 pi 0 2.4]);
ylabel('F1');xlabel('w');

nodist
