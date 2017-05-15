%Spectral density of transformer signal
[y1,fs1]=wavread('transformer1.wav'); %read wav file

Ny=length(y1);
tiv=1/fs1;
t=0:tiv:((Ny-1)*tiv); %time intervals set

ff1=fft(y1,fs1); %Fourier transform
plot(abs(ff1(1:400)),'k');
title('spectral density');
xlabel('Hz')

