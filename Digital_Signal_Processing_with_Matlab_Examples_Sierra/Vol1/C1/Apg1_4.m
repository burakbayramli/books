% Square signal 
fy=100; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s
duy=0.03; %signal duration in seconds

fs=20000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(duy-tiv); %time intervals set

y=square(wy*t); %signal data set
plot(t,y,'k'); %plots figure
axis([0 duy -1.5 1.5]);
xlabel('seconds'); title('square signal');

