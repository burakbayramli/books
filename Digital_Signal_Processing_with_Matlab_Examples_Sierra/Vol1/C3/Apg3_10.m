% Time-domain response to sine, example B
R=0.5; C=0.1; L=0.1; %values of the components
num=[R*C 0]; % transfer function numerator;
den=[L*C R*C 1]; %transfer function denominator
G=tf(num,den); %transfer function
% Input sine signal 
fu=20; %signal frequency in Hz
wu=2*pi*fu; %signal frequency in rad/s
fs=2000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(2-tiv); %time intervals set (2 seconds)
u=sin(wu*t); %input signal data set
[y,ty]=lsim(G,u,t); %computes the system output
plot(t,y,'k'); %plots output signal
xlabel('seconds'); title('time-domain response to sine, example B');
