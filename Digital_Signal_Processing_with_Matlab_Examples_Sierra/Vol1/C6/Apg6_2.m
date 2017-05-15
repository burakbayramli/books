% Sound of a sine signal with decay
fy=500; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=5000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(4-tiv); %time intervals set (4 seconds)

KT=0.7; %decay constant
y=exp(-KT*t).*sin(wy*t); %signal data set
sound(y,fs); %sound

plot(t,y,'g'); %plots figure
axis([0 4 -1.5 1.5]);
xlabel('seconds'); title('sound of sine signal with decay');


