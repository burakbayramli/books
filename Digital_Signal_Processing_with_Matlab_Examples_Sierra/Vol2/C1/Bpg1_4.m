% Simple half-band FIR example
% 
h0=[0.1 0 -0.3 0 0.2 0.5 0.2 0 -0.3 0 0.1]; %original causal half-band filter.
w=0:(2*pi/511):pi;
H0=(fft(h0,512)); %discrete Fourier transform
MH0=abs(H0);

plot(w,MH0(1:256),'k'); 
axis([0 pi 0 1.6]);
title('frequency response (magnitude)');
xlabel('w');
