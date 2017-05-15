% Time-domain response to square signal, example B
R=0.5; C=0.1; L=0.1; %values of the components
num=[R*C 0]; % transfer function numerator;
den=[L*C R*C 1]; %transfer function denominator
G=tf(num,den); %transfer function
% Input square signal 
fu=0.2; %signal frequency in Hz
wu=2*pi*fu; %signal frequency in rad/s
fs=2000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(20-tiv); %time intervals set (20 second)
u=square(wu*t); %input signal data set
[y,ty]=lsim(G,u,t); %computes the system output
subplot(2,1,1); plot(t,u,'k'); %plots input signal
axis([0 20 -1.2 1.2]);
ylabel('input'); title('time-domain response to square, example B');
subplot(2,1,2); plot(t,y,'k'); %plots output signal
ylabel('output'); xlabel('seconds'); 
