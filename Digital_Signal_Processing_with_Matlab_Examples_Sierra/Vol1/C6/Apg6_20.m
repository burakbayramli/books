%Spectrogram of siren signal
[y1,fs1]=wavread('srn.wav'); %read wav file
soundsc(y1,fs1); %hear the signal
specgram(y1,256,fs1); %spectrogram
title('spectrogram of siren signal')
