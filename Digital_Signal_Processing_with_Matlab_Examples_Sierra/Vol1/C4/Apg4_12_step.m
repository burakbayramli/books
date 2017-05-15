% Step response of Chebyishev 2 filter
wc=10; % desired cut-off frequency
N=5; % order of the filter
R=20; %decibels of ripple in the stop band
[num,den]=cheby2(N,R,wc,'s'); %analog Chebyshev 2 filter
G=tf(num,den); %transfer function
step(G,'k'); %step response of G
title('step response of 5th Chebyshev 2 filter');
