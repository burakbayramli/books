function bld060201
% Kap VI.2, Beispiel 3, Potential
clf, clc
subplot(1,2,1)
% -- X-Achse -------------
c = 0.18;d = 0.08;
X = [-1,2]; Y = [0,0];
arrow(X,Y,c,d,'k',2)
% -- Y-Achse -------------
X = [0,0]; Y = [-1.5,1.5];
arrow(X,Y,c,d,'k',2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%
X = linspace(-1,1.6,80);
Y = (X.*X.*X - X.*X)/2;
plot(X,Y,'k','linewidth',2)
text(1.8,-0.15,'x','Fontsize',22)
text(0.1,1.3,'U','Fontsize',22)
axis equal tight
grid off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subplot(1,2,2)
% Kap VI.2, Beispiel 3, Phasenbild
set(gcf,'renderer','zbuffer')
hidden on
% -- X-Achse ---------------------
X = [-1,2]; Y = [0,0];
arrow(X,Y,c,d,'k',2)
% -- Y-Achse -------------
X = [0,0]; Y = [-1.5,1.5];
arrow(X,Y,c,d,'k',2)
%%%%%%%%%%%%%%%%%%%%%%%%
C = 0;
X = linspace(0,1,20);
Y = sqrt(C + X.*X - X.*X.*X);
Z = - Y;
plot(X,Y,'r','linewidth',2),hold on
plot(X,Z,'r','linewidth',2),hold on
%%%%%%%%%%%%%%%%%%%%%%%%
C = 9/8;
X = linspace(-0.5,1.5,30);
Y = sqrt(C + X.*X - X.*X.*X);
Z = - Y;
plot(X,Y,'b','linewidth',2),hold on
plot(X,Z,'b','linewidth',2),hold on
%%%%%%%%%%%%%%%%%%%%%%%%
C = -1/8;
X = linspace(0.5,2,150);
AUX = C + X.*X - X.*X.*X;
J = find(AUX >= 0);
X = X(J);
Y = sqrt(AUX(J));
X = [X, X(length(X))];
Y = [Y, 0];
Z = - Y;
plot(X,Y,'g','linewidth',2),hold on
plot(X,Z,'g','linewidth',2),hold on
%%%%%%%%%%%%%%%%%%%%%%%%
C = 0;
X = linspace(-1,0.5,20);
AUX = C + X.*X - X.*X.*X;
J = find(AUX >= 0);
X = X(J);
X = X(J);
Y = sqrt(AUX(J));
Z = - Y;
plot(X,Y,'r','linewidth',2),hold on
plot(X,Z,'r','linewidth',2),hold on
%%%%%%%%%%%%%%%%%%%%%%%%
C = -3/8;
X = linspace(-1,-0.5,30);
AUX = C + X.*X - X.*X.*X;
J = find(AUX >= 0);
X = X(J);
Y = sqrt(AUX(J));
Z = - Y;
plot(X,Y,'b','linewidth',2),hold on
plot(X,Z,'b','linewidth',2),hold on
%%%%%%%%%%%%%%%%%%%%%%%%
C = 25/64;
X = linspace(-1,5/4,30);
AUX = C + X.*X - X.*X.*X;
J = find(AUX >= 0);
X = X(J);
Y = sqrt(AUX(J));
Z = - Y;
plot(X,Y,'b','linewidth',2),hold on
plot(X,Z,'b','linewidth',2),
%%%%%%%%%%%%%%%%%%%%%%%%
circle(2/3,0,0.04,'w')
circle(0,0,0.04,'w')
text(1.8,-0.2,'x','Fontsize',22)
text(0.1,1.35,'dx/dt','Fontsize',22)
axis equal tight
grid off




