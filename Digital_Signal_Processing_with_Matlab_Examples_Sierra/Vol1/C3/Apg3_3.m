% Frequency response of example A
R=1; C=0.1; %values of the components
num=[1]; % transfer function numerator;
den=[R*C 1]; %transfer function denominator
w=logspace(-1,2); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
FI=angle(G); %take phases (rad.)
polar(FI,abs(G)); %plots frequency response
title('frequency response of example A')

