% Step response of a discrete system
num=[1]; den=[1 -0.5]; %transfer function numerator and denominator
Ts=0.1; %sampling period
G=tf(num,den,Ts); %transfer function
step(G,'k'); %step response of G
title('step response of discrete system')
