% Demodulation of  pulses
% the modulating signal is a sine
fa=40; %signal frequency in Hz
wa=2*pi*fa; %signal frequency in rad/s

nsc=20; %number of samples per signal period
fs=nsc*fa; %sampling frequency in Hz
tiv=1/fs; %time interval between samples;
t=0:tiv:(0.03-tiv); %time intervals set (0.03 seconds)

%sampling the modulating signal
a=0.5+0.3*sin(wa*t);

subplot(3,1,1);
plot(t,a,'k');
ylabel('modulating sine'); title('demodulation of pulses');
axis([0 0.025 -0.2 1.2]);

%PWM modulation
[PWMy,ty]=modulate(a,fa,fs,'pwm','centered');
%PWM demodulation
PWMa=demod(PWMy,fa,fs,'pwm','centered');
subplot(3,1,2)
plot(t,PWMa,'k');
ylabel('PWM');
axis([0 0.025 -0.2 1.2]);

%PTM modulation
[PTMy,ty]=modulate(a,fa,fs,'ptm',0.1);
%PTM demodulation
PTMa=demod(PTMy,fa,fs,'ptm');
subplot(3,1,3)
plot(t,PTMa,'k');
ylabel('PTM');
axis([0 0.025 -0.2 1.2]);
xlabel('seconds');



