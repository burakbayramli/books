%Spectrogram of Big-Ben signal
[y1,fs1]=wavread('chime_big_ben1.wav'); %read wav file

R=3;
y1r=decimate(y1,R); %decimate the audio signal
soundsc(y1r,fs1/R); %hear the decimated signal

specgram(y1r,512,fs1/R); %spectrogram
title('spectrogram of 1/3 decimated Big-Ben signal')
