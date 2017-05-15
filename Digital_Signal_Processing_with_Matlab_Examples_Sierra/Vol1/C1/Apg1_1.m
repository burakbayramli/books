% Square signal 
A=[1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0];
fs=10; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(3-tiv); %time intervals set (30 values)
plot(t,A,'*k'); %plots figure
axis([0 3 -0.5 1.5]);
xlabel('seconds'); title('square wave samples');

