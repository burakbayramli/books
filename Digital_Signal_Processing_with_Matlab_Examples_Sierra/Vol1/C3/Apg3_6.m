% Step response of example A
R=1; C=0.1; %values of the components
num=[1]; % transfer function numerator;
den=[R*C 1]; %transfer function denominator
G=tf(num,den); %transfer function
step(G,'k'); %step response of G
title('step response of example A')
