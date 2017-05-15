% Comparison of frequency response phase of Butterworth, Chebishev 2 and 
% elliptic filters in polar plane
wc=10; %desired cut-off frequency
N=5; %order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[num,den]=butter(N,wc,'s'); %analog Butterworth filter
w=logspace(-1,3,2000); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
polar(ph,abs(G),'k'); %polar plot
hold on;
ylabel('Phase'); xlabel('rad/s'); title('comparison of frequency response phase');

[num,den]=cheby2(N,Rs,wc,'s'); %analog Chebyshev 2 filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
polar(ph,abs(G),'g'); %polar plot

[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
G=freqs(num,den,w); %computes frequency response
ph=angle(G); %phase
polar(ph,abs(G),'b'); %polar plot

