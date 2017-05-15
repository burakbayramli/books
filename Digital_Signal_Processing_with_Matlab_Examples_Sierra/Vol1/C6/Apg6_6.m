% Hilbert and the frequency of sine signal with frequency variation
fy=15; %signal central frequency in Hz
fs=2000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(1-tiv); %time intervals set (1 seconds)

x=(2*t)-1; %frequency control ramp

y=vco(x,fy,fs); %signal data set
g=hilbert(y); %Hilbert transform of y
dg=diff(g)/tiv; %aprox. derivative
w=abs(dg); %frequency in rad/s
v=w/(2*pi); %to Hz

subplot(2,1,1)
plot(t,y,'k'); %plots the signal
axis([0 1 -1.1 1.1]);
xlabel('seconds')
title('sine with frequency variation');
subplot(2,1,2)
plot(t(2:fs),v,'k'); %plots frequency
axis([0 1 0 30]);
xlabel('seconds');
title('frequency of the sine signal in Hz');


