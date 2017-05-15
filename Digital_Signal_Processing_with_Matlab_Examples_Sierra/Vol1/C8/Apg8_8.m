% Frequency modulation of audio sine signal
fa=2; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=500; %carrier frequency in Hz
wc=2*pi*fc; %carrier frequency in rad/s

fs=3000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(5-tiv); %time intervals set (5 seconds)

beta=20; %modulation depth
y=cos((wc*t)+(beta*sin(wa*t))); %modulated signal data set

%instantaneous frequency estimation
gy=hilbert(y);
dg=diff(gy)/tiv;
w=abs(dg);
v=w/(2*pi);
Nv=length(v);

plot(t(2:Nv+1),v,'k'); %plots frequency of modulated signal
axis([0.05 5 400 550]);
xlabel('seconds'); title('frequency of modulated signal');

%5 seconds of sound
sound(y,fs);


