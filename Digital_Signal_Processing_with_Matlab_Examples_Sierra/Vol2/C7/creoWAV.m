%Leo wav y suena
%el wav tiene que estar en PCM
%Empleo audio converter, para 32000 samples/s, 8bit

[y1,fs1]=wavread('barb1.wav'); %read wav file

R=1;
fsr=fs1/R;
y1r=decimate(y1,R); %decimate the audio signal
N=210000; %signal length
soundsc(y1r(1:N),fsr); %hear the decimated signal

wavwrite(y1r(1:N),fsr,8,'C:\aaa\b1.wav');