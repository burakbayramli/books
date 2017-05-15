%Leo wav y suena
%el wav tiene que estar en PCM
%Empleo audio converter, para 32000 samples/s, 8bit

[y1,fs1]=wavread('please_stand_by.wav'); %read wav file

R=1;
fsr=fs1/R;
y1r=decimate(y1,R); %decimate the audio signal
%N=length(y1r);
N=8492/R; %signal length
soundsc(y1r(1:N),fsr); %hear the decimated signal

wavwrite(y1r(1:N),fsr,8,'C:\aaa\spch2.wav');