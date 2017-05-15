% Modulated signal

fs=100; %sampling frequency in Hz
tiv=1/fs; %time between samples
t=tiv:tiv:(10+tiv); %time intervals set (10 seconds)(t>0)

fsig=5; %signal base frequency in Hz
wsig=fsig*2*pi; %signal base frequency in rad/s
K=1.4; %modulation exponent
y=exp(-i*wsig*(t.^K))'; %the modulated signal

yr=real(y);
plot(t,yr,'k');
axis([0 2 -1 1]);
title('Modulated signal (first 2 seconds)');
xlabel('seconds');
