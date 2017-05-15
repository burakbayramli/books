%analyse vowel with cepstrum
[y1,fs1]=wavread('i.wav'); %read wav file
Ny=length(y1);
tiv=1/fs1;
t=0:tiv:((Ny-1)*tiv); %time intervals set

cz=rceps(y1); %real cepstrum
plot(t(1:300),abs(cz(1:300)),'k'); %plots the signal
title('vowel sound cepstrum'); xlabel('seconds');


