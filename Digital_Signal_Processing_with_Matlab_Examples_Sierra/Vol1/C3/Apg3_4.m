% Frequency response of example B
R=0.5; C=0.1; L=0.1; %values of the components
num=[R*C 0]; % transfer function numerator;
den=[L*C R*C 1]; %transfer function denominator
w=logspace(-1,3); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
AG=20*log10(abs(G)); %take decibels
FI=angle(G); %take phases (rad)
subplot(2,1,1); semilogx(w,AG,'k'); %plots decibels
grid;
ylabel('dB'); title('frequency response of example B')
subplot(2,1,2); semilogx(w,FI,'k'); %plots phases
grid;
ylabel('rad.'); xlabel('rad/s')
