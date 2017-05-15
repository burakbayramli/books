% Response to square signal, low-pass filter
R=1; C=0.1;%values of the components
num=[1]; % transfer function numerator;
den=[R*C 1]; %transfer function denominator
G=tf(num,den); %transfer function
% Input square signal 
fu=7; %signal frequency in Hz
wu=2*pi*fu; %signal frequency in rad/s
fs=2000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(2-tiv); %time intervals set (2 seconds)
u=square(wu*t); %input signal data set
[y,ty]=lsim(G,u,t); %computes the system output
plot(t(3001:4000),y(3001:4000),'k'); %plots last 1/2 second of output signal
xlabel('seconds'); title('response to square signal, low-pass filter');
