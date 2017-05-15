%Quadratic chirp signal
f0=5; %initial frequency in Hz
f1=60; %final frequency in Hz
fs=5000; %sampling rate in Hz
tiv=1/fs; %time between samples
t1=2; %final time
t=0:tiv:(t1-tiv); %time intervals set (2 seconds)
f=0:1:((fs/2)-1); %frequency intervals set

yr=chirp(t,f0,t1,f1,'quadratic')'; %the chirp signal

plot(t,yr,'k');
title('quadratic chirp signal'); xlabel('sec');