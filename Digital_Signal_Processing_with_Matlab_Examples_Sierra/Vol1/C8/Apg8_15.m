% AM and noise in the communication channel

% the modulating signal is a sine
fa=40; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=1500; %carrier frequency in Hz
fs=30000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.03-tiv); %time intervals set (0.03 seconds)

%modulating signal
a=sin(wa*t);

subplot(3,1,1);
plot(t,a,'k');
ylabel('modulating sine'); title('noise in the AM communication');
axis([0 0.025 -1.2 1.2]);

%AM modulation
y=modulate(a,fc,fs,'amdsb-tc');

Nnu=length(y); %number of data points
nu=randn(Nnu,1); %random noise signal data set
yn=y+(0.2*nu)'; %adding noise to communication channel

subplot(3,1,2)
plot(t,yn,'k');
ylabel('comm. signal');
axis([0 0.025 -2.4 2.4]);

%PTM demodulation
da=demod(yn,fc,fs,'amdsb-tc',0.5);
subplot(3,1,3)
plot(t,da,'k');
ylabel('demodulated sine');
axis([0 0.025 -1.2 1.2]);
xlabel('seconds');



