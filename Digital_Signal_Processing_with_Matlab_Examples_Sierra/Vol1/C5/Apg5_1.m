% Frequency response of analog filter example
%Analog filter (wc= 63rad/s = 10Hz.):
num=[63]; % transfer function numerator;
den=[1 63]; %transfer function denominator
w=logspace(0,3); %logaritmic set of frequency values in rad/s.
G=freqs(num,den,w); %computes frequency response
AG=20*log10(abs(G)); %take decibels
FI=angle(G); %take phases (rad)
f=w/(2*pi); %frequencies in Hz.
subplot(2,1,1); semilogx(f,AG,'k'); %plots decibels
grid; axis([1 100 -25 5]);
ylabel('dB'); title('frequency response of analog filter example')
subplot(2,1,2); semilogx(f,FI,'k'); %plots phases
grid;axis([1 100 -1.5 0]);
ylabel('rad.'); xlabel('Hz.')
