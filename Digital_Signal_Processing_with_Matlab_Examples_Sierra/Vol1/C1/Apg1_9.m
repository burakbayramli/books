% Multiplication of sines signal 
fx=70; %signal frequency in Hz
wx=2*pi*fx; %signal frequency in rad/s

fz=2; %signal frequency in Hz
wz=2*pi*fz; %signal frequency in rad/s

fs=6000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(8-tiv); %time intervals set (8 seconds)

y=sin(wx*t).*sin(wz*t);  %signal data set

sound(y,fs); %sound

t=0:tiv:(1-tiv); %time intervals set (1 second)
y=sin(wx*t).*sin(wz*t);  %signal data set
plot(t,y,'k'); %plots figure
axis([0 1 -1.5 1.5]);
xlabel('seconds'); title('multiplication of sines signal');


