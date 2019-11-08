function h = ODE_plotter(t,Y,methodname)

strend1 = ['End point temp = ',num2str(Y(end,1))];
strend2 = ['End point gradient = ',num2str(Y(end,2))];

R=0.2;

h = figure();
subplot(2,1,1)
plot(R - t,Y(:,1),'b.-')
title(strcat('Results using ', methodname))
ylabel('Temperature (^o K)')
xlabel('Radius (m)')
text(R-t(end),Y(end,1),strend1)

subplot(2,1,2)
plot(R - t,Y(:,2),'b.-')
ylabel('Temperature Gradient (^oK/m)')
xlabel('Radius (m)')
text(R-t(end),Y(end,2),strend2)

end