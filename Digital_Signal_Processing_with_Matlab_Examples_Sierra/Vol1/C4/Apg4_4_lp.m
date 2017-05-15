% Abstract design, low-pass filter
num=[10]; % transfer function numerator;
den=[1 10]; %transfer function denominator
w=logspace(-1,2); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
AG=20*log10(abs(G)); %take decibels
semilogx(w,AG,'y'); %plots decibels
axis([0.1 100 -20 10]);
grid;
ylabel('dB'); xlabel('rad/s'); title('frequency response of desired low-pass filter');
