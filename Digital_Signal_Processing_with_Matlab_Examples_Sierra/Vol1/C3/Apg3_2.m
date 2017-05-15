% Frequency response of example A
R=1; C=0.1; %values of the components
num=[1]; % transfer function numerator;
den=[R*C 1]; %transfer function denominator
w=logspace(-1,2); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
AG=20*log10(abs(G)); %take decibels
FI=angle(G); %take phases (rad)
subplot(2,1,1); semilogx(w,AG,'k'); %plots decibels
grid;axis([0.1 100 -25 1]);
ylabel('dB'); title('frequency response of example A')

subplot(2,1,2); semilogx(w,FI,'k'); %plots phases
grid;axis([0.1 100 -1.5 0.2]);
ylabel('rad.'); xlabel('rad/s')
