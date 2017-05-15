% Square signal 
A=[1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0];
fs=500; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(3-tiv); %time intervals set (30 values)
y=square(2*pi*t); y=(y/2)+0.5;
plot(t,y,'-k'); %plots figure
axis([0 3 -0.5 1.5]);
xlabel('seconds'); title('square wave');
