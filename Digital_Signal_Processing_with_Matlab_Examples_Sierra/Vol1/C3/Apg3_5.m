% Frequency response of example B
R=0.5; C=0.1; L=0.1; %values of the components
num=[R*C 0]; % transfer function numerator;
den=[L*C R*C 1]; %transfer function denominator
w=logspace(-1,3); %logaritmic set of frequency values
G=freqs(num,den,w); %computes frequency response
FI=angle(G); %take phases (rad.)
polar(FI,abs(G)); %plots frequency response
title('frequency response of example B')

