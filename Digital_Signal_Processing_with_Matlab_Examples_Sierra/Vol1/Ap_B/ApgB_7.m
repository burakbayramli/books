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
fh=120/(fs/2); %desired cut-off of a low-pass filter
N=5; % order of the filter
[numd,dend]=butter(N,fh); %digital low-pass Butterworth filter
y0=filtfilt(numd,dend,u); %response of the low-pass filter

% extracting the 3rd harmonic
fl=220/(fs/2); % desired low cut-off frequency
fh=380/(fs/2); % desired high cut-off frequency
fb=[fl fh]; %the pass band of the filter
N=10; % order of the filter (5+5)
[numd,dend]=butter(N,fb); %digital band-pass Butterworth filter
y3=filtfilt(numd,dend,u); %response of the band-pass filter

% extracting the 5th harmonic
fh=420/(fs/2); % desired high cut-off frequency in Hz
N=5; % order of the filter
[numd,dend]=butter(N,fh,'high'); %digital high-pass Butterworth filter
y5=filtfilt(numd,dend,u); %response of the high-pass filter

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

