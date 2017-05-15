% Sine signal sound
fy=900; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=6000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(5-tiv); %time intervals set (5 seconds)

y=sin(wy*t); %signal data set
sound(y,fs); %sound

t=0:tiv:(0.01-tiv); %time intervals set (0.01 second)
y=sin(wy*t); %signal data set
plot(t,y,'k'); %plots figure
axis([0 0.01 -1.5 1.5]);
xlabel('seconds'); title('sine signal');


