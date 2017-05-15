% Comparison of frequency response of Chebyshev 2 filters
wc=10; % desired cut-off frequency
N=2; % order of the filter
R=20; %ripple dB in the stop band
[num,den]=cheby2(N,R,wc,'s'); %analog Chebyshev 2 filter
w=logspace(0,2,400); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
hold on;
axis([1 100 0 1.1]);
grid;
ylabel('Gain'); xlabel('rad/s'); title('frequency response of Chebyshev 2 filter');
for N=4:2:8, % more orders of the filter
[num,den]=cheby2(N,R,wc,'s'); %analog Chebyshev 2 filter
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
end;
