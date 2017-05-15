% Filter specifications
wc=10; %desired cut-off frequency
N=4; %order of the filter
Rp=1; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
w=logspace(0,2,400); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]);
grid;
ylabel('Gain'); xlabel('rad/s'); title('filter specifications');
