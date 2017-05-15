% Frequency response of high-pass filter
R=1; C=0.1; %values of the components
num=[C 0]; % transfer function numerator;
den=[R*C 1]; %transfer function denominator
w=logspace(-1,2); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
AG=20*log10(abs(G)); %take decibels
semilogx(w,AG,'k'); %plots decibels
axis([0.1 100 -30 10]);
grid;
ylabel('dB'); xlabel('rad/s'); title('frequency response of high-pass filter');
