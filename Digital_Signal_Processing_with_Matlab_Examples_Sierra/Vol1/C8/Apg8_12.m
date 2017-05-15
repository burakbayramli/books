% Amplitude modulation of sine signal, using modulate()
fa=80; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=1500; %carrier frequency in Hz

fs=30000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.03-tiv); %time intervals set (0.03 seconds)

a=sin(wa*t);
y=modulate(a,fc,fs,'amdsb-tc');

plot(t,y,'k'); %plots modulated signal
axis([0 0.03 -2.2 2.2]);
xlabel('seconds'); title('amplitude modulation of sine signal');

