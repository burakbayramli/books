%sawtooth signal to be analyzed
fy=1; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s
Ty=1/fy; %signal period in seconds
Ns=256; %number of samples per signal period
fs=Ns*fy; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:((3*Ty)-tiv); %time intervals set (3 periods)
y3=sawtooth(wy*t); %signal data set
plot(t,y3,'k');
xlabel('seconds'); title('sawtooth signal (3 periods)');

