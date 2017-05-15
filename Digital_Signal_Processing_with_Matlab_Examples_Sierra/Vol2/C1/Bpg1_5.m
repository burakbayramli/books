% Simple half-band FIR pair example
% 
h0=[0.1 0 -0.3 0 0.2 0.5 0.2 0 -0.3 0 0.1]; %original causal half-band filter.
h1=[-0.1 0 0.3 0 -0.2 0.5 -0.2 0 0.3 0 -0.1]; %a mirror filter is obtained by sign alternation.

w=0:(2*pi/511):pi;
H0=(fft(h0,512)); %discrete Fourier transform
H1=(fft(h1,512)); %discrete Fourier transform
HT=H0+H1;
MH0=abs(H0);
MH1=abs(H1);
MHT=abs(HT);

plot(w,MH0(1:256),'kx'); hold on;
plot(w,MH1(1:256),'k');
plot(w,MHT(1:256),'b');
axis([0 pi 0 1.6]);
title('frequency response (magnitude)');
xlabel('w');
