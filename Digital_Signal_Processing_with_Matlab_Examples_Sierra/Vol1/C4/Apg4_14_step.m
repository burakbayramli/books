% Step response of elliptic filter
wc=10; % desired cut-off frequency
N=5; % order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
G=tf(num,den); %transfer function
step(G,'k'); %step response of G
title('step response of 5th elliptic filter');
