% Frequency response of a QMF
c=1/sqrt(2);
h0=[c c]; %low-pass filter
h1=[c -c]; %high-pass filter
w=0:(2*pi/511):pi;
H0=real(fft(h0,512)); %discrete Fourier transform
H1=real(fft(h1,512)); % "  "  "
plot(w,H0(1:256),'k',w(1:8:256),H0(1:8:256),'kx'); hold on;
plot(w,H1(1:256),'k');
axis([0 pi 0 1.5]);
title('frequency response of QMF H0 and H1 filters');
xlabel('w');
