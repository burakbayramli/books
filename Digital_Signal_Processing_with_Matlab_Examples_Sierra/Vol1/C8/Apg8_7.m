% Frequency modulation of sine signal
fa=40; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=1000; %carrier frequency in Hz
wc=2*pi*fc; %carrier frequency in rad/s

fs=30000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.1-tiv); %time intervals set (0.1 seconds)

beta=20; %modulation depth
y=cos((wc*t)+(beta*sin(wa*t))); %modulated signal data set

plot(t,y,'k'); %plots modulated signal
axis([0.05 0.1 -1.5 1.5]);
xlabel('seconds'); title('frequency modulation of sine signal');

