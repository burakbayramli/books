% Comparison of frequency response phase of Bessel filters
wc=10; % desired cut-off frequency
N=2; % order of the filter
[num,den]=besself(N,wc); %analog Bessel filter
w=logspace(-1,3); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
semilogx(w,180*unwrap(ph)/pi,'r'); %plots phase
hold on;
axis([0.1 1000 -800 90]);
grid;
ylabel('Phase'); xlabel('rad/s'); title('frequency response phase of Bessel filter');
for N=4:2:8, % more orders of the filter
[num,den]=besself(N,wc); %analog Bessel filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
semilogx(w,180*unwrap(ph)/pi,'k'); %plots phase
end;
