%chirp-z transform of a signal

fs=200; %sampling rate in Hz
t=0:(1/fs):1; %time intervals set (1 seconds)

f1=10; %sine1 frequency in Hz
f2=11; %sine2 frequency in Hz
y=sin(2*pi*f1*t)+sin(2*pi*f2*t); %sum of two sine signals
Ny=length(y);

subplot(2,1,1)
fy=0:(fs/Ny):fs;
sy=fft(y); %the Fourier transform
plot(fy(1:50),abs(sy(1:50)),'k');
title('Fourier transform'); xlabel('Hz');

subplot(2,1,2)
cf1=5; cf2=25; %in Hz
m=128; %number of contour points
w=exp(-j*(2*pi*(cf2-cf1))/(m*fs)); %ratio between contour points
a=exp(j*(2*pi*cf1)/fs); %contour starting point

chy=czt(y,m,w,a); %the chirp-z transform
fhiv=(cf2-cf1)/m; %frequency interval
fhy=cf1:fhiv:(cf2-fhiv);
plot(fhy,abs(chy),'k');
title('chirp-z transform'); xlabel('Hz');


