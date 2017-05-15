% Filtering the sine+noise signal
fs=4000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(4-tiv); %time intervals set (4 seconds)
N=length(t); %number of data points
yr=0.5*randn(N,1); %random signal data set
fy=400; %sinusoidal signal frequency (400 Hz)
ys=sin(fy*2*pi*t); %sinusoidal signal
y=ys+yr'; %the signal+noise
subplot(2,1,1); plot(t(1:400),y(1:400),'k'); %plots sine+noise (first 0.1 sec)
axis([0 0.1 -2 2]);
ylabel('signal+noise');
title('filtering the sine+noise signal');

fl=370; % desired low cut-off frequency in Hz
fh=430; % desired high cut-off frequency in Hz
wl=fl*2*pi; wh=fh*2*pi; % to rad/s
wb=[wl wh]; %the pass band of the filter
N=10; % order of the filter (5+5)
[num,den]=butter(N,wb,'s'); %analog Butterworth filter
G=tf(num,den); %transfer function of the filter

yout=lsim(G,y,t); %filter output
subplot(2,1,2); plot(t(1:400),yout(1:400),'k'); %plots extracted signal (first 0.1 sec.)
axis([0 0.1 -2 2]);
xlabel('seconds'); ylabel('extracted signal');

sound(y,fs); %sound of sine+noise
pause(5);
sound(yout,fs); %sound of extracted signal


