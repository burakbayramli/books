% Sinusoidal output and input 
G=tf([600],[1 90 600]); %the transfer function G(s)

% Input sine signal 
fu=50; %signal frequency in Hz
wu=2*pi*fu; %signal frequency in rad/s
fs=2000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(1-tiv); %time intervals set (1 second)
u=sin(wu*t); %input signal data set
subplot(2,1,1); plot(t(1901:2000),u(1901:2000),'k'); %plots input signal
ylabel('input');title('input and output');

% Output sine signal
[y,ty]=lsim(G,u,t); %computes the system output
subplot(2,1,2); plot(t(1901:2000),y(1901:2000),'k'); %plots output signal
ylabel('output'); xlabel('seconds')
