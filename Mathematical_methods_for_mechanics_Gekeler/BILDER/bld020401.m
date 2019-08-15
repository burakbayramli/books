function bld020401
% Abb. 27, Loesung DGL in dgl.m
clc, clf
T = linspace(0,2,120);
X  =  0.5*exp(-2*T) + 0.5*exp(-40*T).*(cos(40*T) + sin(40*T));
plot(T,X,'LineWidth',2),hold on
Y  =  0.5*exp(-2*T) - 0.5*exp(-40*T).*(cos(40*T) + sin(40*T));
plot(T,Y,'LineWidth',2),hold on
Z  = - exp(-40*T).*(cos(40*T) - sin(40*T));
plot(T,Z,'LineWidth',2),hold on
grid on
text(0.05,0.7,'x(t)','Fontsize',26);
text(0.05,0.3,'y(t)','Fontsize',26);
text(0.05,-0.3,'z(t)','Fontsize',26);
axis equal tight
grid off
