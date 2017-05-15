%IIR from impulse response, using arburg
fs=256; %sampling frequency in Hz
fmx=128; %input bandwidth in Hz
F=0:1:fmx-1; %response frequencies 0,1,2...Hz

numd=1;
dend=[1 -0.5 0.1 0.5];
H=freqz(numd,dend,F,fs); %IIR frequency response
[h,th]=impz(numd,dend,64,fs); %impulse response

subplot(1,2,1)
plot(F,abs(H),'rx'); hold on;
N=2; %IIR denominator degree
mdend=arburg(h, N); %IIR filter modelling
mnumd=1;
HM=freqz(mnumd,mdend,F,fs); %IIR model frequency response
plot(F,abs(HM),'k');
title('frequency response'); xlabel('Hz');

subplot(1,2,2)
plot(th,h,'rx'); hold on;
[mh,mth]=impz(mnumd,mdend,64,fs);
plot(mth,mh,'k');
axis([-0.02 0.25 -0.8 1.2]);
title('impulse response'); xlabel('seconds');

