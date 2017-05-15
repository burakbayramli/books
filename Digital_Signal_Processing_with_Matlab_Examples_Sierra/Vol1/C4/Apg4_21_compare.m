% Comparison of frequency response of Bessel filters
wc=10; % desired cut-off frequency
N=2; % order of the filter
[num,den]=besself(N,wc); %analog Bessel filter
w=logspace(0,2); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
hold on;
axis([1 100 0 1.1]);
grid;
ylabel('Gain'); xlabel('rad/s'); title('frequency response of Bessel filter');
for N=4:2:8, % more orders of the filter
[num,den]=besself(N,wc); %analog Bessel filter
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
end;
