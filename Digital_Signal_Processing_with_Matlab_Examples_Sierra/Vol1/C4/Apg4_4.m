% Abstract design, desired filter
num1=conv([1 0.1],[1 25]); num=conv(num1,[1 40]); % transfer function numerator;
den1=conv([1 1],[1 10]); den=conv(den1,[1 100]); %transfer function denominator
w=logspace(-2,3); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
AG=20*log10(abs(G)); %take decibels
semilogx(w,AG,'--b'); %plots decibels
axis([0.01 1000 -25 5]);
grid;
ylabel('dB'); xlabel('rad/s'); title('frequency response of desired filter');
