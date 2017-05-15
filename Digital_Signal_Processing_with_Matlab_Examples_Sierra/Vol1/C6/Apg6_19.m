%Spectrogram of car doppler signal
[y1,fs1]=wavread('doppler.wav'); %read wav file

R=12;
y1r=decimate(y1,R); %decimate the audio signal
soundsc(y1r,fs1/R); %hear the decimated signal

specgram(y1r,256,fs1/R); %spectrogram
title('spectrogram of 1/12 decimated car Doppler signal')
