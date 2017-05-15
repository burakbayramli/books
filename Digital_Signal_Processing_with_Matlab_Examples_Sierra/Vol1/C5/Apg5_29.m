% Comparison of impulse response of the 4 digital filters
fs=130; %sampling frequency in Hz
fc=10/(fs/2); %cut-off at 10 Hz
nsa=50; %number of samples to visualize

N=5; %order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[numd,dend]=butter(N,fc); %digital Butterworth filter
subplot(2,2,1); impz(numd,dend,nsa); %plots impulse response
title('Butterworth');

[numd,dend]=cheby1(N,Rp,fc); %digital Chebyshev 1 filter
subplot(2,2,2); impz(numd,dend,nsa); %plots impulse response
title('Chebyshev 1');

[numd,dend]=cheby2(N,Rs,fc); %digital Chebyshev 2 filter
subplot(2,2,3); impz(numd,dend,nsa); %plots  impulse response
title('Chebyshev 2');

[numd,dend]=ellip(N,Rp,Rs,fc); %digital elliptic filter
subplot(2,2,4); impz(numd,dend,nsa); %plots  impulse response
title('Elliptic');

