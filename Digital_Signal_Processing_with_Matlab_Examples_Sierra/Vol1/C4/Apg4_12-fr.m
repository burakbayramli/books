% Frequency response of Chebyshev 2 filter
wc=10; %desired cut-off frequency
N=5; %order of the filter
R=20; %decibels of ripple in the stop band
[num,den]=cheby2(N,R,wc,'s'); %analog Chebyshev 2 filter
w=logspace(0,2,400); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]);
grid;
ylabel('Gain'); xlabel('rad/s'); title('frequency response of 5th Chebyshev 2 filter');
