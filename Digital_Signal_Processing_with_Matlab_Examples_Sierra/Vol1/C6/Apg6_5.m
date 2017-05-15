% Sound of sine signal with frequency variation
fy=600; %signal central frequency in Hz
fs=6000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(4-tiv); %time intervals set (4 seconds)

x=(t/2)-1; %frequency control ramp (-1 to 1)

y=vco(x,fy,fs); %signal data set
sound(y,fs); %sound

plot(t,y,'k'); %plots figure
axis([0 0.5 -1.1 1.1]);
xlabel('seconds'); title('sound of sine signal with frequency variation');


