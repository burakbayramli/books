% Comparison of frequency response of the 4 filters
wc=10; %desired cut-off frequency
N=5; %order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[num,den]=butter(N,wc,'s'); %analog Butterworth filter
w=logspace(0,2,400); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
subplot(2,2,1); semilogx(w,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]); grid;
ylabel('Gain'); xlabel('rad/s'); title('Butterworth');

[num,den]=cheby1(N,Rp,wc,'s'); %analog Chebyshev 1 filter
G=freqs(num,den,w); %computes frequency response
subplot(2,2,2); semilogx(w,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]); grid;
ylabel('Gain'); xlabel('rad/s'); title('Chebyshev 1');

[num,den]=cheby2(N,Rs,wc,'s'); %analog Chebyshev 2 filter
G=freqs(num,den,w); %computes frequency response
subplot(2,2,3); semilogx(w,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]); grid;
ylabel('Gain'); xlabel('rad/s'); title('Chebyshev 2');

[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
G=freqs(num,den,w); %computes frequency response
subplot(2,2,4); semilogx(w,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]); grid;
ylabel('Gain'); xlabel('rad/s'); title('Elliptic');

