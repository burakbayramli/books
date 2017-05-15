% Frequency response of Chebyshev 1 filter
wc=10; %desired cut-off frequency
N=5; %order of the filter
R=0.5; %decibels of ripple in the pass band
[num,den]=cheby1(N,R,wc,'s'); %analog Chebyshev 1 filter
w=logspace(0,2,400); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]);
grid;
ylabel('Gain'); xlabel('rad/s'); title('frequency response of 5th Chebyshev 1 filter');
