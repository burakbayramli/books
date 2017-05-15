% Adding and recovering experiment
fs=4000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.1-tiv); %time intervals set (0.1 seconds)
fu0=100; %base sinusoidal signal frequency (100 Hz)
u0=sin(fu0*2*pi*t); %fundamental harmonic
u3=sin(3*fu0*2*pi*t); %3rd harmonic
u5=sin(5*fu0*2*pi*t); %5th harmonic
u= u0 + (0.5*u3) + (0.3*u5); %input signal

% extracting the fundamental harmonic
fh=120; %desired cut-off of a low-pass filter
wh=fh*2*pi; % to rad/s
N=5; % order of the filter
[num,den]=butter(N,wh,'s'); %analog low-pass Butterworth filter
G=tf(num,den); %transfer function of the filter
y0=lsim(G,u,t); %response of the low-pass filter

% extracting the 3rd harmonic
fl=280; % desired low cut-off frequency in Hz
fh=320; % desired high cut-off frequency in Hz
wl=fl*2*pi; wh=fh*2*pi; % to rad/s
wb=[wl wh]; %the pass band of the filter
N=10; % order of the filter (5+5)
[num,den]=butter(N,wb,'s'); %analog band-pass Butterworth filter
G=tf(num,den); %transfer function of the filter
y3=lsim(G,u,t); %response of the band-pass filter

% extracting the 5th harmonic
fh=480; % desired high cut-off frequency in Hz
wh=fh*2*pi; % to rad/s
N=5; % order of the filter
[num,den]=butter(N,wh,'high','s'); %analog high-pass Butterworth filter
G=tf(num,den); %transfer function of the filter
y5=lsim(G,u,t); %response of the high-pass filter

figure(1)
subplot(4,1,1); plot(t,u,'k'); %the complete signal
ylabel('compound signal');
title('adding and recovering experiment');

subplot(4,1,2); plot(t,y0,'k'); %the recovered fundamental harmonic
ylabel('y0');
subplot(4,1,3); plot(t,y3,'k'); %the recovered 3rd harmonic
ylabel('y3');
subplot(4,1,4); plot(t,y5,'k'); %the recovered 5th harmonic
ylabel('y5');
xlabel('seconds');

%------------------------
ysum=y0+y3+y5; %adding recovered harmonics

figure(2)
subplot(2,1,1); plot(t,u,'k'); %the complete input signal
ylabel('compound signal');
title('adding and recovering experiment');

subplot(2,1,2); plot(t,ysum,'k'); %the added harmonics
ylabel('ysum');
xlabel('seconds');

