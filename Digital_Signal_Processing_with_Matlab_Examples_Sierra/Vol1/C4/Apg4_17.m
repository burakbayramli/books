% Comparison of step response of the 4 filters
wc=10; % desired cut-off frequency
N=5; % order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[num,den]=butter(N,wc,'s'); %analog Butterworth filter
G=tf(num,den); %transfer function
subplot(2,2,1); step(G,'k'); %step response of G
title('Butterworth');

[num,den]=cheby1(N,Rp,wc,'s'); %analog Chebyshev 1 filter
G=tf(num,den); %transfer function
subplot(2,2,2); step(G,'k'); %step response of G
title('Chebyshev 1');

[num,den]=cheby2(N,Rs,wc,'s'); %analog Chebyshev 2 filter
G=tf(num,den); %transfer function
subplot(2,2,3); step(G,'k'); %step response of G
title('Chebyshev 2');

[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
G=tf(num,den); %transfer function
subplot(2,2,4); step(G,'k'); %step response of G
title('Elliptic');

