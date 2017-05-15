% Simple mirror FIR example
% a second FIR filter is obtained by sign alternation
h0=[1 3 1 4 1 3 1]; %original filter with symmetrical coeffs.
h1=[1 -3 1 -4 1 -3 1]; %a second filter is obtained.

w=0:(2*pi/511):pi;
H0=abs(fft(h0,512)); %discrete Fourier transform
H1=abs(fft(h1,512)); %discrete Fourier transform

plot(w,H0(1:256),'kx'); hold on;
plot(w,H1(1:256),'k');
axis([0 pi 0 16]);
title('frequency response (magnitude)');
xlabel('w');
