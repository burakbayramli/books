%sinc function 
fy=1; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s
Ty=1/fy; %signal period in seconds

Ns=256; %number of samples per signal period
fs=Ns*fy; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=-((6*Ty)-tiv):tiv:((6*Ty)-tiv); %time intervals set (12 periods)

y=sinc(t); %signal data set
plot(t,y); hold on;
plot([0 0],[-0.4 1.2],'k');
xlabel('seconds'); title('sinc function');


