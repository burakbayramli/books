% Step response of example B
R=0.5; C=0.1; L=0.1; %values of the components
num=[R*C 0]; % transfer function numerator;
den=[L*C R*C 1]; %transfer function denominator
G=tf(num,den); %transfer function
step(G,'k'); %step response of G
title('step response of example B')

