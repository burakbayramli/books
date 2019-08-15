function bld030303
% Ellipse, wahre und exzentrische Anomalie
%
clc, clear
clf, hold on
% -- Rahmen --------------
%XR = [-1.3,1.3,1.3,-1.3,-1.3];
%YR = [-1.3,-1.3,1.3,1.3,-1.3];
%plot(XR,YR,'k','linewidth',2), hold on
plot(1.3,1.3,'w.'), hold on
plot(-1.3,-1.3,'w.'), hold on
axis equal tight, axis manual
% -- ellipse ----------------
a = 1; b = 0.7;
e = sqrt(a*a - b*b);
X = linspace(-1,1,100); Y = b*sqrt(1 - X.*X/a^2);
Z = -b*sqrt(1 - X.*X/a^2);

plot(X,Y,'k','linewidth',2), hold on
plot(X,Z,'k','linewidth',2), hold on
% -- Kreis -------------
Y = sqrt(1 - X.*X/a^2);
Z = -sqrt(1 - X.*X/a^2);
plot(X,Y,'k','linewidth',2), hold on
plot(X,Z,'k','linewidth',2), hold on
% -- Strecken -------------------
PHI = pi/3;
X = [0, a*cos(PHI)]; Y = [0, a*sin(PHI)];
plot(X,Y,'k--','linewidth',2), hold on
X = [a*cos(PHI), a*cos(PHI)]; Y = [0, a*sin(PHI)];
plot(X,Y,'k--','linewidth',2), hold on
P1 = a*cos(PHI); P2 = b*sqrt(1 - P1*P1/a^2);
X = [e, P1]; Y = [0, P2];
plot(X,Y,'k','linewidth',2), hold on

c = 0.15;d = 0.05;
X = [-1.2, 1.2];
Y = [0, 0];
arrow(X,Y,c,d,'k',2),hold on
c = 0.09;
d = 0.03;
X = [0.08, 0.08];
Y = [-0.15,0.05];
arrow(X,Y,c,d,'k',1),hold on

X = [e+0.08,e+0.08];
Y = [-0.15,0.05];
arrow(X,Y,c,d,'k',1),hold on
rr = 0.03;
circle(0,0,rr,'w')
circle(e,0,rr,'w')
circle(a*cos(PHI),a*sin(PHI),rr,'w')
circle(P1,P2,rr,'w')
circle(P1,0,rr,'w')
X4 = [e;0]; Y4 = [e + 0.13;0]; PSI = 1.9;
[X5,Y5] = segm(X4,Y4,PSI,0);
plot(X5,Y5), hold on
X6 = [0;0]; Y6 = [0.13;0]; PSI = 1.1;
[X7,Y7] = segm(X6,Y6,PSI,0);
plot(X7,Y7), hold on

text(0.66,0.3,'r','Fontsize',24)
text(0.02,-0.26,'\psi','Fontsize',24)
text(0.7,-0.26,'\phi','Fontsize',24)
grid off
axis off
