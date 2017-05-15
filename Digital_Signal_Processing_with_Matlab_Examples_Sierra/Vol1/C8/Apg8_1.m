% Amplitude modulation of sine signal
fa=80; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=1500; %carrier frequency in Hz
wc=2*pi*fc; %carrier frequency in rad/s

fs=30000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.03-tiv); %time intervals set (0.03 seconds)

MD=0.4; %modulation depth
A=1+(MD*sin(wa*t)); %amplitude
y=A.*sin(wc*t); %modulated signal data set

plot(t,y,'k'); %plots modulated signal
axis([0 0.03 -1.5 1.5]);
xlabel('seconds'); title('amplitude modulation of sine signal');

