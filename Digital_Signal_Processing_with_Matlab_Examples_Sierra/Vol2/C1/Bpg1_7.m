% Simple orthogonal FIR pair example
% a second FIR filter is obtained by flipping and sign alternation
h0=[1 2 3 4 5 6]; %original non-symmetrical filter.
h1=[-6 5 -4 3 -2 1]; %a second filter is obtained.

w=0:(2*pi/511):pi;
H0=abs(fft(h0,512)); %discrete Fourier transform
H1=abs(fft(h1,512)); %discrete Fourier transform

plot(w,H0(1:256),'kx'); hold on;
plot(w,H1(1:256),'k');
axis([0 pi 0 22]);
title('frequency responses (magnitude)');
xlabel('w');
