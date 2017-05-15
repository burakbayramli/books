% Pieces of 4-PSK

fc=1000; %carrier frequency in Hz
ns=20; %number of samples per carrier cycle
fs=ns*fc; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:((1/fc)-tiv); %time intervals set, 1 sine period

%carrier signals for 1 bit time
c1=sin(2*pi*fc*t);
c2=sin((2*pi*fc*t)+(pi/2));
c3=sin((2*pi*fc*t)+(pi));
c4=sin((2*pi*fc*t)+(3*pi/2));

%plots of the four modulated PSK pieces
subplot(2,2,1)
plot(t,c1,'k'); %plots 0 phase modulated signal
ylabel('0 phase'); xlabel('seconds');
title('pieces of 4-PSK modulation of sine signal');
axis([0 1/fc -1.2 1.2]);

subplot(2,2,2)
plot(t,c2,'k'); %plots 1 phase modulated signal
ylabel('1 phase'); xlabel('seconds');
axis([0 1/fc -1.2 1.2]);

subplot(2,2,3)
plot(t,c3,'k'); %plots 2 phase modulated signal
ylabel('2 phase'); xlabel('seconds');
axis([0 1/fc -1.2 1.2]);

subplot(2,2,4)
plot(t,c4,'k'); %plots 3 phase modulated signal
ylabel('3 phase');xlabel('seconds');
axis([0 1/fc -1.2 1.2]);


