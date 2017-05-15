% Spectrogram of sine signal with frequency variation
fy=30; %signal central frequency in Hz
fs=500; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(10-tiv); %time intervals set (10 seconds)

x=((2*t)/10)-1; %frequency control ramp (-1 to 1)

y=vco(x,fy,fs); %signal data set

specgram(y,256,fs); %plots spectrogram
axis([0 8 0 70]);
title('spectrogram of the sine signal with frequency variation');

