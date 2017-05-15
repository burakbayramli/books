%Piano note modelling, using stmcb
[y1,fs1]=wavread('piano-G6.wav'); %read wav file
R=3;
y1r=decimate(y1,R); %decimate the audio signal
soundsc(y1r);
[y2,fs2]=wavread('piano-C6.wav'); %read wav file
R=3;
y2r=decimate(y2,R); %decimate the audio signal
soundsc(y2r);

disp('computing G6 model');
% y1r is considered as the impulse response of a IIR filter
% let us get a model of this "filter"
na=40; %IIR denominator degree
nb=40; %IIR numerator degree
[mnumd,mdend]=stmcb(y1r,nb,na); %IIR filter modelling
[h1,th1]=impz(mnumd,mdend,length(y1r),fs1/R); %impulse response of the IIR model
subplot(2,1,1); plot(th1,h1); title('G6 model');
soundsc(h1); %hearing the impulse response

disp('computing C6 model');
% y2r is considered as the impulse response of a IIR filter
% let us get a model of this "filter"
na=40; %IIR denominator degree
nb=40; %IIR numerator degree
[mnumd,mdend]=stmcb(y2r,nb,na); %IIR filter modelling
[h2,th2]=impz(mnumd,mdend,length(y2r),fs2/R); %impulse response of the IIR model
subplot(2,1,2); plot(th2,h2); title('C6 model');
soundsc(h2); %hearing the impulse response

pause(1);

for n=1:5,
soundsc(h1); %hearing the impulse response
pause(0.4);
soundsc(h2); %hearing the impulse response
pause(0.4);
end