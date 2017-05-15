% Pole-zero map of a discrete system
num=[1 -0.5]; den=[1 0.5 0.8]; %transfer function numerator and denominator
Ts=0.1; %sampling period
G=tf(num,den,Ts); %transfer function
pzmap(G); %pole-zero map
zgrid

