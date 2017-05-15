% SSB amplitude modulation of of sine signal
fa=80; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=1500; %carrier frequency in Hz
wc=2*pi*fc; %carrier frequency in rad/s

fs=30000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.05-tiv); %time intervals set (0.05 seconds)

A=sawtooth(wa*t); %modulating signal
gA=hilbert(A); %Hilbert transform of modulating signal
C=cos(wc*t); %carrier signal
gC=hilbert(C); %Hilbert transform of carrier
gSSB=gA.*gC; %multiplication
y=real(gSSB); %real part

subplot(2,1,1)
plot(t,A,'k'); %plots modulating signal
axis([0.01 0.04 -1.2 1.2]);
ylabel('a(t)');
title('SSB amplitude modulation with sawtooth signal')

subplot(2,1,2)
plot(t,y,'k'); %plots modulated signal
axis([0.01 0.04 -3 3]);
xlabel('seconds'); ylabel('y(t)');
