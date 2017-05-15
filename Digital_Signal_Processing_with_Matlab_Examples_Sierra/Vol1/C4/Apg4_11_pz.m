% Pole-zero map of Chebyshev 1 filter
wc=10; % desired cut-off frequency
N=5; % order of the filter
R=0.5; %decibels of ripple in the pass band
[num,den]=cheby1(N,R,wc,'s'); %analog Chebyshev 1 filter 
G=tf(num,den); %transfer function
P=pole(G); %find the poles of G
alfa=-(pi/2):-0.1:-(3*pi/2); %set of angle values
plot(wc*cos(alfa),wc*sin(alfa),'--'); %plots half a circunference
hold on;
plot(P,'dk'); %pole map
title('pole-zero map of 5th Chebyshev 1 filter');
