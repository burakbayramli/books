% frequency response of sgolay filter
fs=300; %sampling frequency in Hz.

K=6; %polynomial order
FR=25; %frame size
%input signal
fu=3; %signal frequency in Hz
wu=2*pi*fu; %signal frequency in rad/s
tiv=1/fs; %time intervals between samples
t=0:tiv:(1-tiv); %time intervals set (1 seconds)
nn=length(t); %number of data points

us=sawtooth(wu*t); %sawtooth signal
ur=randn(nn,1); %random signal
u=us+0.16*ur'; %the signal+noise

%filter output
y=sgolayfilt(u,K,FR); %filter output signal
%figure
subplot(2,1,1)
plot(t,us,'k');
axis([0 1 -2 2]);
title('Savitzky-Golay filter')

subplot(2,1,2)
plot(t,u,'b'); hold on;
plot(t,y,'r')
axis([0 1 -2 2]);
xlabel('seconds');


