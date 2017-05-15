%analyze AM modulation with cepstrum

% AM sine signal
fa=100; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

fc=1000; %carrier frequency in Hz
wc=2*pi*fc; %carrier frequency in rad/s

fs=10000; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.1-tiv); %time intervals set (0.5 seconds)
Nt=length(t);

MD=0.5; %modulation depth
A=1+(MD*sin(wa*t)); %amplitude
y=A.*sin(wc*t); %modulated signal data set

subplot(2,1,1)
ysum=1.5*A+real(y); %signals adding
plot(t(2:Nt/2),ysum(2:Nt/2),'k'); %plots added signals
title('added signals'); 
axis([0 0.05 0 6]);

subplot(2,1,2)
cz=rceps(y); %real cepstrum
plot(t(2:Nt/2),abs(cz(2:Nt/2)),'k'); %plots the cepstrum
title('cepstrum of AM signal'); xlabel('seconds');
axis([0 0.05 0 0.3]);



