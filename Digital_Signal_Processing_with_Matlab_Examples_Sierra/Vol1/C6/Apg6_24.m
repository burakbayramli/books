%Spectral densities of quack signal begin and end
[y1,fs1]=wavread('duck_quack.wav'); %read wav file

Ny=length(y1);
tiv=R/fs1;
t=0:tiv:((Ny-1)*tiv); %time intervals set

subplot(2,1,1)
y1beg=y1(1:Ny/3); %first 1/3 of signal
ff1=fft(y1beg,fs1); %Fourier transform
plot(abs(ff1(1:2500)),'k');
title('first 1/3 of quack: spectral density');
xlabel('Hz')

subplot(2,1,2)
y1end=y1(2*Ny/3:Ny); %last 1/3 of signal
ff3=fft(y1end,fs1); %Fourier transform
plot(abs(ff3(1:2500)),'k');
title('last 1/3 of quack: spectral density');
xlabel('Hz')


