function bld060202
% Kap VI.2, Beispiel 4, Potential
clf, clc
subplot(1,2,1)
% -- X-Achse ----------------
c = 0.22;d = 0.08;
X = [-1.8,1.8];Y = [0,0];
arrow(X,Y,c,d,'k',2)
% -- Y-Achse -----------------
X = [0,0];Y = [-1.5,1.5];
arrow(X,Y,c,d,'k',2)
% -- Potential ---------------
X = linspace(-1.5,1.5,50);
Y = (X.*X.*X.*X - X.*X)/2;
plot(X,Y,'k','linewidth',2)
text(1.5,-0.2,'x','Fontsize',18)
text(0.1,1.25,'U','Fontsize',18)
axis equal tight
grid off
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
subplot(1,2,2)
% -- X-Achse ---------------
X = [-1.8,1.8];Y = [0,0];
arrow(X,Y,c,d,'k',2)
% -- Y-Achse ----------
X = [0,0];Y = [-1.5,1.5];
arrow(X,Y,c,d,'k',2)
%-- Homokline Orbits -----------
C = 0;
X = linspace(0,1,40);
AUX = C + X.*X - X.*X.*X.*X;
J = find(AUX >= 0);
X = X(J);
X = X(J);
Y = sqrt(AUX(J));
Z = - Y;
plot(X,Y,'r','linewidth',2),hold on
plot(X,Z,'r','linewidth',2),hold on
%-- Homokline Orbits ---------------
C = 0;
X = linspace(-1,0,20);
AUX = C + X.*X - X.*X.*X.*X;
J = find(AUX >= 0);
X = X(J);
X = X(J);
Y = sqrt(AUX(J));
Z = - Y;
plot(X,Y,'r','linewidth',2),hold on
plot(X,Z,'r','linewidth',2),hold on
%-- Periodischer Orbit rechts innen-----
C = -7/64;
X = linspace(0.06,1,100);
AUX = C + X.*X - X.*X.*X.*X;
J = find(AUX >= 0);
X = X(J);
Y = sqrt(AUX(J));
X = [X,X(length(X))];
Y = [Y,0];
Z = - Y;
plot(X,Y,'g','linewidth',2),hold on
plot(X,Z,'g','linewidth',2),hold on
%-- Periodischer Orbit links innen ---
C = -7/64;
X = linspace(-0.06,-1,100);
AUX = C + X.*X - X.*X.*X.*X;
J = find(AUX >= 0);
X = X(J);
Y = sqrt(AUX(J));
X = [X,X(length(X))];
Y = [Y,0];
Z = - Y;
plot(X,Y,'g','linewidth',2),hold on
plot(X,Z,'g','linewidth',2),hold on
%-- Periodischer Orbit rechts aussen --------
C = 1/2;
X = linspace(0,1.5,50);
AUX = C + X.*X - X.*X.*X.*X;
J = find(AUX >= 0);
X = X(J);
Y = sqrt(AUX(J));
X = [X,X(length(X))];
Y = [Y,0];
Z = - Y;
plot(X,Y,'b','linewidth',2),hold on
plot(X,Z,'b','linewidth',2),hold on
%-- Periodischer Orbit links aussen -------
C = 1/2;
X = linspace(0,-1.5,50);
AUX = C + X.*X - X.*X.*X.*X;
J = find(AUX >= 0);
X = X(J);
Y = sqrt(AUX(J));
X = [X,X(length(X))];
Y = [Y,0];
Z = - Y;
plot(X,Y,'b','linewidth',2),hold on
plot(X,Z,'b','linewidth',2),hold on
axis equal tight
grid off
circle(1/sqrt(2),0,0.05,'w')
circle(-1/sqrt(2),0,0.05,'w')
circle(0,0,0.05,'w')
text(1.5,-0.2,'x','Fontsize',22)
text(0.1,1.2,'dx/dt','Fontsize',22)
