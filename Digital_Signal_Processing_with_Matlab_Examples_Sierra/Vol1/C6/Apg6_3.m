%Hilbert and the envelope of sine signal with decay
fy=40; %signal frequency in Hz
wy=2*pi*fy; %signal frequency in rad/s

fs=2000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(1-tiv); %time intervals set (1 seconds)

KT=3; %decay constant
y=exp(-KT*t).*sin(wy*t); %signal data set

g=hilbert(y); %Hilbert transform of y
m=abs(g); %complex modulus

plot(t,y,'b'); hold on; %plots figure
plot(t,m,'k',t,-m,'k'); %plots envelope

axis([0 1 -1.1 1.1]);
xlabel('seconds'); title('envelope of sine signal with decay');


