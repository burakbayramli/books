% PTM and noise in the communication channel

% the modulating signal is a sine
fa=40; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

nsc=60; %number of samples per signal period
fs=nsc*fa; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.03-tiv); %time intervals set (0.03 seconds)

%sampling the modulating signal
a=0.5+0.3*sin(wa*t);

subplot(3,1,1);
plot(t,a,'k');
ylabel('modulating sine'); title('noise in the PTM communication');
axis([0 0.025 -0.2 1.2]);

%PTM modulation
[PTMy,ty]=modulate(a,fa,fs,'ptm',0.1);

Nnu=length(ty); %number of data points
nu=randn(Nnu,1); %random noise signal data set
PTMyn=PTMy+(0.2*nu)'; %adding noise to communication channel

subplot(3,1,2)
plot(ty/nsc,PTMyn,'k');
ylabel('comm. signal');
axis([0 0.025 -0.8 1.8]);

%PTM demodulation
PTMa=demod(PTMyn,fa,fs,'ptm');
subplot(3,1,3)
plot(t,PTMa,'k');
ylabel('demodulated sine');
axis([0 0.025 -0.2 1.2]);
xlabel('seconds');




