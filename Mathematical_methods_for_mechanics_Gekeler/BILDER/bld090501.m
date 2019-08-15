function bld090501
% Singulaere Elemente, Kurve fuer g
% s = 1/2; phi - alfa = pi/2;
clc, clf, clear
r0 = 1/2; r1 = 1; s = 2/3;
X = linspace(0,r0,60);
A = sin(s);
Y = A*X.^s;
plot(X,Y,'k','linewidth',2), hold on
g0 = r0^s; g1 = s*r0^(s-1);
X = linspace(r0,r1,60);
C = -(3*g0 + 2*g1*(r1 - r0))/(r1 - r0)^2;
D = (2*g0 + g1*(r1 - r0))/(r1 - r0)^3;
g = g0 + g1*(X - r0) + C*(X - r0).^2 + D*(X - r0).^3;
Y = A*g;
plot(X,Y,'k','linewidth',2), hold on
X = [r0,r0]; Y = [0,0.45];
plot(X,Y,'k','linewidth',2)
grid off
text(0.65,0.4,'\phi-\alpha = \pi/2','fontsize',28)
text(0.7,0.35,'s = 2/3','fontsize',28)
text(0.52,0.03,'r_0','fontsize',28)
text(0.94,0.03,'r_1','fontsize',28)
