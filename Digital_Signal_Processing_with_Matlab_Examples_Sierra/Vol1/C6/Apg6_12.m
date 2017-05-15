%analyse vowel with spectrum
[y1,fs1]=wavread('i.wav'); %read wav file
Ny=length(y1);

sz=fft(y1); %spectrum
fiv=fs1/Ny;
fz=0:fiv:((0.5*fs1)-fiv); %frequency interval set
plot(fz(1:500),abs(sz(1:500)),'k'); %plots part of the spectrum
title('vowel sound spectrum');
xlabel('Hz')

