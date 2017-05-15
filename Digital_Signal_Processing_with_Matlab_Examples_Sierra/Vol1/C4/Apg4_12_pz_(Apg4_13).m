% Pole-zero map of Chebyshev 2 filter
wc=10; % desired cut-off frequency
N=5; % order of the filter
R=20; %decibels of ripple in the stop band
[num,den]=cheby2(N,R,wc,'s'); %analog Chebyshev 2 filter 
G=tf(num,den); %transfer function
P=pole(G); %find the poles of G
Z=zero(G); %find the zeros of G
alfa=-(pi/2):-0.1:-(3*pi/2); %set of angle values
plot(wc*cos(alfa),wc*sin(alfa),'--'); %plots half a circunference
hold on;
plot(P,'dk'); %pole map
axis([-18 2 -15 15]);
hold on;
plot(Z,'ok'); %zero map
title('pole-zero map of 5th Chebyshev 2 filter');
