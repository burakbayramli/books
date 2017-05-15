%IIR from impulse response, the four methods
fs=256; %sampling frequency in Hz
fmx=128; %input bandwidth in Hz
F=0:1:fmx-1; %response frequencies 0,1,2...Hz

numd=1;
dend=[1 -0.5 0.1 0.5];
figure(1)
H=freqz(numd,dend,F,fs); %IIR frequency response
[h,th]=impz(numd,dend,64,fs); %impulse response

subplot(2,2,1)
plot(F,abs(H),'rx'); hold on
N=3; %IIR denominator degree
mdend=arcov(h, N); %IIR filter modelling
mnumd=1;
HM=freqz(mnumd,mdend,F,fs); %IIR model frequency response
plot(F,abs(HM),'k');
title('arcov() modelling'); xlabel('Hz');

subplot(2,2,2)
plot(F,abs(H),'rx'); hold on
N=6; %IIR denominator degree
mdend=armcov(h, N); %IIR filter modelling
mnumd=1;
HM=freqz(mnumd,mdend,F,fs); %IIR model frequency response
plot(F,abs(HM),'k');
title('armcov() modelling'); xlabel('Hz');

subplot(2,2,3)
plot(F,abs(H),'rx'); hold on
N=2; %IIR denominator degree
mdend=arburg(h, N); %IIR filter modelling
mnumd=1;
HM=freqz(mnumd,mdend,F,fs); %IIR model frequency response
plot(F,abs(HM),'k');
title('arburg() modelling'); xlabel('Hz');

subplot(2,2,4)
plot(F,abs(H),'rx'); hold on
N=3; %IIR denominator degree
mdend=aryule(h, N); %IIR filter modelling
mnumd=1;
HM=freqz(mnumd,mdend,F,fs); %IIR model frequency response
plot(F,abs(HM),'k');
title('aryule() modelling'); xlabel('Hz');

