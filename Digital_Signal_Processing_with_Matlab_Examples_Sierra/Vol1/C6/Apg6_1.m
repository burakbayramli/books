% Sine signal with decay
fy=40; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=2000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(1-tiv); %time intervals set (1 seconds)

KT=3; %decay constant
y=exp(-KT*t).*sin(wy*t); %signal data set

plot(t,y,'k'); %plots figure
axis([0 1 -1.1 1.1]);
xlabel('seconds'); title('sine signal with decay');


