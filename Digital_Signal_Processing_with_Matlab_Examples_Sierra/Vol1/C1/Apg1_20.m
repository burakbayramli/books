% Sine signal & aliasing
fy=3; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

% good sampling fequency
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(3-tiv); %time intervals set
y=sin(wy*t); %signal data set
subplot(2,1,1); plot(t,y,'k'); %plots figure
axis([0 3 -1.5 1.5]);
title('3Hz sine signal');
ylabel('fs=100');

% too slow sampling fequency
fs=4; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(3-tiv); %time intervals set
y=sin(wy*t); %signal data set
subplot(2,1,2); plot(t,y,'-kd'); %plots figure
axis([0 3 -1.5 1.5]);
xlabel('seconds'); 
ylabel('fs=4');

