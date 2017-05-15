% Sine signal with frequency variation
fy=15; %signal central frequency in Hz
fs=2000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(1-tiv); %time intervals set (1 seconds)

x=(2*t)-1; %frequency control ramp (-1 to 1)

y=vco(x,fy,fs); %signal data set

plot(t,y,'k'); %plots figure
axis([0 1 -1.1 1.1]);
xlabel('seconds'); title('sine signal with frequency variation');


