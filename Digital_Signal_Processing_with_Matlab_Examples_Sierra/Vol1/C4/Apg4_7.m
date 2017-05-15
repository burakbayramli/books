% Step response of Butterworth filter
wc=10; % desired cut-off frequency
N=5; % order of the filter
[num,den]=butter(N,wc,'s'); %analog Butterworth filter
G=tf(num,den); %transfer function
step(G,'k'); %step response of G
title('step response of 5th Butterworth filter');
