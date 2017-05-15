% Sum of sines signal 
fy=300; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=6000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(5-tiv); %time intervals set (5 seconds)

y=0.6*sin(wy*t)+0.3*sin(3*wy*t)+0.2*sin(5*wy*t); %signal data set

sound(y,fs); %sound

t=0:tiv:(0.01-tiv); %time intervals set (0.01 second)
nn=length(t);
plot(t,y(1:nn),'k'); %plots figure
axis([0 0.01 -1.5 1.5]);
xlabel('seconds'); title('sum of sines signal');


