% Comparison of frequency response of the 4 digital filters
fs=130; %sampling frequency in Hz
fc=10/(fs/2); %cut-off at 10 Hz

N=5; %order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[numd,dend]=butter(N,fc); %digital Butterworth filter
F=logspace(0,2,400); %logaritmic set of frequency values in Hz
G=freqz(numd,dend,F,fs); %computes frequency response
subplot(2,2,1); semilogx(F,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]); grid;
ylabel('Gain'); xlabel('Hz'); title('Butterworth');

[numd,dend]=cheby1(N,Rp,fc); %digital Chebyshev 1 filter
G=freqz(numd,dend,F,fs); %computes frequency response
subplot(2,2,2); semilogx(F,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]); grid;
ylabel('Gain'); xlabel('Hz'); title('Chebyshev 1');

[numd,dend]=cheby2(N,Rs,fc); %digital Chebyshev 2 filter
G=freqz(numd,dend,F,fs); %computes frequency response
subplot(2,2,3); semilogx(F,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]); grid;
ylabel('Gain'); xlabel('Hz'); title('Chebyshev 2');

[numd,dend]=ellip(N,Rp,Rs,fc); %digital elliptic filter
G=freqz(numd,dend,F,fs); %computes frequency response
subplot(2,2,4); semilogx(F,abs(G),'k'); %plots linear amplitude
axis([1 100 0 1.1]); grid;
ylabel('Gain'); xlabel('Hz'); title('Elliptic');

