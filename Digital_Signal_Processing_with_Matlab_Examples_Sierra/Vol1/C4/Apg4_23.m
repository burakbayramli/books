% Comparison of frequency response phase of 5 filters
wc=10; %desired cut-off frequency
N=5; %order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[num,den]=butter(N,wc,'s'); %analog Butterworth filter
w=logspace(-1,3,500); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
semilogx(w,180*unwrap(ph)/pi,'k'); %plots phase
hold on;
axis([0.1 1000 -500 90]);
grid;
ylabel('Phase'); xlabel('rad/s'); title('comparison of frequency response phase of the filters');

[num,den]=cheby1(N,Rp,wc,'s'); %analog Chebyshev 1 filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
semilogx(w,180*unwrap(ph)/pi,'r'); %plots phase

[num,den]=cheby2(N,Rs,wc,'s'); %analog Chebyshev 2 filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
semilogx(w,180*unwrap(ph)/pi,'g'); %plots phase

[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
semilogx(w,180*unwrap(ph)/pi,'b'); %plots phase

[num,den]=besself(N,wc); %analog Bessel filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
semilogx(w,180*unwrap(ph)/pi,'m'); %plots phase
