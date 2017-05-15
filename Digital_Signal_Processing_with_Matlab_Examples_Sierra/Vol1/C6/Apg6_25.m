%Triangle signal
[y1,fs1]=wavread('triangle1.wav'); %read wav file
soundsc(y1,fs1);
Ny=length(y1); %number of signal samples
tiv=1/fs1; %sampling time interval
t=0:tiv:((Ny-1)*tiv); %time data set
plot(t,y1,'g'); %plots the signal
title('triangle sound'); xlabel('seconds')
