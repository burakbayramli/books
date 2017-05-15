%Spectrogram of harp signal
[y1,fs1]=wavread('harp1.wav'); %read wav file
%soundsc(y1,fs1); %hear the signal

R=2;
y1r=decimate(y1,R); %decimate the audio signal
soundsc(y1r,fs1/R); %hear the decimated signal

[sgy,fy,ty]=specgram(y1r,512,fs1/R); %spectrogram computation
contour(ty,fy,abs(sgy)); %plots the spectrogram
title('spectrogram of 1/2 decimated harp signal')
xlabel('seconds'); ylabel('Hz');
axis([0 3 0 2000]);
