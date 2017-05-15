% Comparison of pole-zero maps of the 4 digital filters
fs=130; %sampling frequency in Hz
fc=10/(fs/2); %cut-off at 10 Hz

N=5; %order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[numd,dend]=butter(N,fc); %digital Butterworth filter
tfd=tf(numd,dend);
[P,Z]=pzmap(tfd); %poles and zeros of the filter
subplot(2,2,1);
%plots pole-zero map
plot(P,'kx','Markersize',10); hold on; 
plot(Z,'ko','Markersize',8);
zgrid; axis([-1.1 1 -1 1]);
title('Butterworth');

[numd,dend]=cheby1(N,Rp,fc); %digital Chebyshev 1 filter
tfd=tf(numd,dend);
[P,Z]=pzmap(tfd); %poles and zeros of the filter
subplot(2,2,2); 
%plots pole-zero map
plot(P,'kx','Markersize',10); hold on; 
plot(Z,'ko','Markersize',8); 
zgrid; axis([-1.1 1 -1 1]);
title('Chebyshev 1');

[numd,dend]=cheby2(N,Rs,fc); %digital Chebyshev 2 filter
tfd=tf(numd,dend);
[P,Z]=pzmap(tfd); %poles and zeros of the filter
subplot(2,2,3);
%plots pole-zero map
plot(P,'kx','Markersize',10); hold on; 
plot(Z,'ko','Markersize',8); 
zgrid; axis([-1.1 1 -1 1]);
title('Chebyshev 2');

[numd,dend]=ellip(N,Rp,Rs,fc); %digital elliptic filter
tfd=tf(numd,dend);
[P,Z]=pzmap(tfd); %poles and zeros of the filter
subplot(2,2,4);
%plots pole-zero map
plot(P,'kx','Markersize',10); hold on; 
plot(Z,'ko','Markersize',8); 
zgrid; axis([-1.1 1 -1 1]);
title('Elliptic');

