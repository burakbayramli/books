% Response of Chebyshev 1 filter to square signal near cut-off
wc=10; % desired cut-off frequency
N=5; % order of the filter
Rp=0.5; %pass-band ripple
[num,den]=cheby1(N,Rp, wc,'s'); %analog Chebyshev 1 filter
G=tf(num,den); %transfer function
fs=100; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(6-tiv); %time intervals set (6 seconds)

% Input square signal 1
wu=5; %signal frequency in rad/s
u=square(wu*t); %input signal data set
[y,ty]=lsim(G,u,t); %computes the system output
subplot(3,1,1); plot(t,y,'k'); %plots output signal
axis([0 6 -1.5 1.5]); ylabel('5 rad/s')
title('response to square signal, 5th Chebyshev 1 filter');

% Input square signal 2
wu=10; %signal frequency in rad/s
u=square(wu*t); %input signal data set
[y,ty]=lsim(G,u,t); %computes the system output
subplot(3,1,2); plot(t,y,'k'); %plots output signal
axis([0 6 -1.5 1.5]); ylabel('10 rad/s')

% Input square signal 3
wu=15; %signal frequency in rad/s
u=square(wu*t); %input signal data set
[y,ty]=lsim(G,u,t); %computes the system output
subplot(3,1,3); plot(t,y,'k'); %plots output signal
axis([0 6 -1.5 1.5]); ylabel('15 rad/s')

xlabel('seconds'); 

