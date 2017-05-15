% Response to square signal, high-pass filter
R=1; C=0.1;%values of the components
num=[C 0]; % transfer function numerator;
den=[R*C 1]; %transfer function denominator
G=tf(num,den); %transfer function
% Input square signal 
fu=0.1; %signal frequency in Hz
wu=2*pi*fu; %signal frequency in rad/s
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(60-tiv); %time intervals set (60 seconds)
u=square(wu*t); %input signal data set
[y,ty]=lsim(G,u,t); %computes the system output
plot(t(2001:6000),y(2001:6000),'k'); %plots last 40 seconds of output signal
axis([20 60 -3 3]);
xlabel('seconds'); title('response to square signal, low-pass filter');
