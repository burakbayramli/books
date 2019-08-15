% Eulersche Knicklast, Fall 1
clear, clc, clf
c = 0.6;  % Laenge Pfeilspitze
d = 0.2;  % Breite Pfeilspitze
RR = 0.15; % Radius Kreis
X = [-1,1,1,-1,-1]; Y = [-1,-1,0,0,-1];
fill(X,Y,'y','edgecolor','y'), hold on
RS = RR + 0.05;
X = [-RS,-RS,-0.5,-0.5,-RS];
Y = [5.5,6.5,6.5,5.5,5.5];
fill(X,Y,'y','edgecolor','y'), hold on
X = [RS,RS,0.5,0.5,RS];
Y = [5.5,6.5,6.5,5.5,5.5];
fill(X,Y,'y','edgecolor','y'), hold on
X = [-RS,-RS]; Y = [5.5,6.5];
plot(X,Y,'k','linewidth',2), hold on
X = [RS,RS]; Y = [5.5,6.5];
plot(X,Y,'k','linewidth',2), hold on

X = [-1,1]; Y = [0, 0];
plot(X,Y,'k','linewidth',2), hold on
X = [0, 0]; Y = [-0.5,6];
plot(X,Y,'k-.','linewidth',2),hold on
X = [0,0]; Y = [7.5,6+RR];
arrow(X,Y,c,d,'k',2)
Y = linspace(0,6,20);

% -- Kurve -------------------5
ll = 6; kappa = 1/2; sqlamda = 2*pi/ll;
Y = linspace(0,6,30);
X = kappa*(1 - cos(sqlamda*Y));
plot(X,Y,'k','linewidth',3), hold on
X = [0,0]; Y = [0,-0.5];
plot(X,Y,'k','linewidth',3), hold on

%-- Rahmen ---------------
X = [-3,3,3,-3,-3]; Y = [-2,-2,8,8,-2];
plot(X,Y,'k','linewidth',2), hold on

%get(h)
axis equal tight, grid on
%circle(0,0,RR,'w')
%circle(0,6,RR,'w')
text(0.5,7,'F','fontsize',25)

axis off
