%Leo wav y suena
%el wav tiene que estar en PCM
%Empleo audio converter, para 32000 samples/s, 8bit

[y1,fs1]=wavread('barb1.wav'); %read wav file

soundsc(y1,fs1); %hear the decimated signal

