% Abstract design, band-pass filter
num=[100 0]; % transfer function numerator;
den=conv([1 10],[1 100]); %transfer function denominator
w=logspace(0,3); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
AG=20*log10(abs(G)); %take decibels
semilogx(w,AG,'y'); %plots decibels
axis([1 1000 -20 10]);
grid;
ylabel('dB'); xlabel('rad/s'); title('frequency response of desired band-pass filter');
