% Comparison of frequency response of elliptic filters
wc=10; % desired cut-off frequency
N=2; % order of the filter
Rp=0.5; %ripple dB in the pass band
Rs=20; %ripple dB in the stop band
[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
w=logspace(0,2,400); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
hold on;
axis([1 100 0 1.1]);
grid;
ylabel('Gain'); xlabel('rad/s'); title('frequency response of elliptic filter');
for N=4:2:8, % more orders of the filter
[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
G=freqs(num,den,w); %computes frequency response
semilogx(w,abs(G),'k'); %plots linear amplitude
end;
