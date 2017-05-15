%STFT of 1-sine signal
fy=50; %signal frequency in Hz
fs=128; %sampling rate in Hz
tiv=1/fs; %time between samples
t1=0:tiv:(2-tiv); %time of first signal part (2 seconds)
tn=2:tiv:(8-tiv); %time for yn (6 seconds)
t2=8:tiv:(10-tiv); %time of last signal part (2 seconds)
y1=0*exp(-j*2*pi*t1);
yn=exp(-j*2*pi*fy*tn);
y2=0*exp(-j*2*pi*t2);

y=[y1 yn y2]'; %complete signal (column vector)
t=[t1 tn t2]; %complete signal time set
f=0:1:((fs/2)-1); %frequency intervals set

[sgy,fy,ty]=specgram(y,512,fs); %spectrogram computation

colmap1; colormap(mapg1); %user colormap
imagesc(ty,fy,log10(0.1+abs(sgy))); axis xy; %plots the spectrogram
title('Spectrogram of 1-sine packet, fine frequency precision');
xlabel('seconds'); ylabel('Hz');

