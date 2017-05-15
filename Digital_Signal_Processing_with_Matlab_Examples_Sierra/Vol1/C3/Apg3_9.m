% Time-domain response to sine, example A
R=1; C=0.1; %values of the components
num=[1]; % transfer function numerator;
den=[R*C 1]; %transfer function denominator
G=tf(num,den); %transfer function
% Input sine signal 
fu=20; %signal frequency in Hz
wu=2*pi*fu; %signal frequency in rad/s
fs=2000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(1-tiv); %time intervals set (1 second)
u=sin(wu*t); %input signal data set
[y,ty]=lsim(G,u,t); %computes the system output
plot(t,y,'k'); %plots output signal
xlabel('seconds'); title('time-domain response to sine, example A');
