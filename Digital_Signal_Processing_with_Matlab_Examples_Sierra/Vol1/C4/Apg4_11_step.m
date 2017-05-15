% Step response of Chebyishev 1 filter
wc=10; % desired cut-off frequency
N=5; % order of the filter
R=0.5; %decibels of ripple in the pass band
[num,den]=cheby1(N,R,wc,'s'); %analog Chebyshev 1 filter 
G=tf(num,den); %transfer function
step(G,'k'); %step response of G
title('step response of 5th Chebyshev 1 filter');
