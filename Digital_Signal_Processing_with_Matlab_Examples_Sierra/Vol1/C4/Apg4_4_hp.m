% Abstract design, high-pass filter
num=[1 0]; % transfer function numerator;
den=[1 100]; %transfer function denominator
w=logspace(0,4); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
AG=20*log10(abs(G)); %take decibels
semilogx(w,AG,'y'); %plots decibels
axis([1 10000 -20 10]);
grid;
ylabel('dB'); xlabel('rad/s'); title('frequency response of desired high-pass filter');
