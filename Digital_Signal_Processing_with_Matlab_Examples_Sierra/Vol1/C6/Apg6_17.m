%Hear & see car doppler WAV
[y1,fs1]=wavread('doppler.wav'); %read wav file
soundsc(y1,fs1); %hear wav

Ny=length(y1);
tiv=1/fs1;
t=0:tiv:((Ny-1)*tiv); %time intervals set

plot(t,y1,'g'); %plots the signal
axis([0 (Ny*tiv) -1.2 1.2]);
title('car doppler sound');
ylabel('signal'); xlabel('seconds')


