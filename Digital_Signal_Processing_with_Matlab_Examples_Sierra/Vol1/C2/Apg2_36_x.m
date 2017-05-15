% Illustration of Inversion Procedure

x=0:0.02:5;
F=1-exp(-x); %the distribution F

plot(x,F,'k'); hold on;
title('Inversion procedure');
ylabel('series U'), xlabel('series Z');

