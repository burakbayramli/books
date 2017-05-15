% Sine signal 
fy=1; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=60; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(3-tiv); %time intervals set

y=sin(wy*t); %signal data set
plot(t,y,'k'); %plots figure
axis([0 3 -1.5 1.5]);
xlabel('seconds'); title('sine signal');

