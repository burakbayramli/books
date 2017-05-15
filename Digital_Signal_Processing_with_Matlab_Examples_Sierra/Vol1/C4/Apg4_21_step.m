% Step response of Bessel filter
wc=10; % desired cut-off frequency
N=5; % order of the filter
[num,den]=besself(N,wc); %analog Bessel filter
G=tf(num,den); %transfer function
t=0:0.01:1.5; %time vector (1.5 seconds)
step(G,t,'k'); %step response of G
title('step response of 5th Bessel filter');
