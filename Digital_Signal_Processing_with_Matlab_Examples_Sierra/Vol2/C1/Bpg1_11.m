% Spectral factorization of a FIR half-band example
% 
hf=[0.1 0 -0.3 0 0.2 1 0.2 0 -0.3 0 0.1]; %original causal half-band filter.
r1=roots(hf);

figure(1)
alfa=0:0.01:(2*pi);
plot(cos(alfa),sin(alfa),'b'); hold on;
plot(r1,'ko'); grid;
axis([-2 2 -1.2 1.2]);
title('zeros of FIR half band filter');

h1=poly(r1(1:5)); %using zeros outside unit circle
h0=poly(r1(6:10)); %using zeros inside unit circle

figure(2)
w=0:(2*pi/511):pi;
H0=abs(fft(h0,512)); %discrete Fourier transform
H1=abs(fft(h1,512));  % "  "  "
plot(w,H0(1:256),'kx'); hold on;
plot(w,H1(1:256),'k');
axis([0 pi 0 15]);
title('frequency response (magnitude) of factor filters');
xlabel('w');

