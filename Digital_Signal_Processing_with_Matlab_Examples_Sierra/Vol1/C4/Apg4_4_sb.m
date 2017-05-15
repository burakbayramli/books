% Abstract design, band-stop filter
num=conv([1 25],[1 40]); % transfer function numerator;
den=conv([1 10],[1 100]); %transfer function denominator
w=logspace(0,3); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
AG=20*log10(abs(G)); %take decibels
semilogx(w,AG,'y'); %plots decibels
axis([1 1000 -20 5]);
grid;
ylabel('dB'); xlabel('rad/s'); title('frequency response of desired band-stop filter');
