% Comparison of frequency response phase of Butterworth, Chebyshev 1 and 
% Bessel filters in polar plane
wc=10; %desired cut-off frequency
N=5; %order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[num,den]=butter(N,wc,'s'); %analog Butterworth filter
w=logspace(-1,3,500); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
polar(ph,abs(G),'k'); %polar plot
hold on;
ylabel('Phase'); xlabel('rad/s'); title('comparison of frequency response phase');

[num,den]=cheby1(N,Rp,wc,'s'); %analog Chebyshev 1 filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
polar(ph,abs(G),'r'); %polar plot

[num,den]=besself(N,wc); %analog Bessel filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
polar(ph,abs(G),'m'); %polar plot
