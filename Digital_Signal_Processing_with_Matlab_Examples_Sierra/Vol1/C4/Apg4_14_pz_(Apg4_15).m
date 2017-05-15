% Pole-zero map of elliptic filter
wc=10; % desired cut-off frequency
N=5; % order of the filter
Rp=0.5; %decibels of ripple in the pass band
Rs=20; %decibels of ripple in the stop band
[num,den]=ellip(N,Rp,Rs,wc,'s'); %analog elliptic filter
G=tf(num,den); %transfer function
P=pole(G); %find the poles of G
Z=zero(G); %find the zeros of G
alfa=-(pi/2):-0.1:-(3*pi/2); %set of angle values
plot(wc*cos(alfa),wc*sin(alfa),'--'); %plots half a circunference
hold on;
plot(P,'dk'); %pole map
hold on;
plot(Z,'ok'); %zero map
title('pole-zero map of 5th elliptic filter');
