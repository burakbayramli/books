%spectrogram of a chirp
f0=1; %initial frequency in Hz
f1=200; %final frequency in Hz
fs=600; %sampling rate in Hz
t=0:(1/fs):20; %time intervals set (20 seconds)
t1=20; %final time;

y = chirp(t,f0,t1,f1,'quadratic'); %the chirp signal

specgram(y,256,fs); %the spectrogram
title('quadratic chirp spectrogram');


