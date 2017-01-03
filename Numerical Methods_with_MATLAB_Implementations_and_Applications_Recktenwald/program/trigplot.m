% trigplot Script to plot sin(x), cos(x), and sin(x)*cos(x)

x = linspace(0,2*pi);    %  generate data
y1 = sin(x);
y2 = cos(x);
y3 = y1.*y2;             %  y3 = sin(x)*cos(x)

plot(t,y1,'-',t,y2,':',t,y3,'--');
axis([0 2*pi -1.5  1.5])
legend('sin(\theta)','cos(\theta)','sin(\theta)*cos(\theta)')
xlabel('\theta (radians)','FontName','Times','FontSize',14)
title('Plot of simple trigonometric functions',...
       'FontName','Times','FontSize',12)
