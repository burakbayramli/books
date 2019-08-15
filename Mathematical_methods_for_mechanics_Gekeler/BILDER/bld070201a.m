% Eulersche Knicklast, Fall 1
clear, clc, clf
c = 0.6;  % Laenge Pfeilspitze
d = 0.2;  % Breite Pfeilspitze
RR = 0.15; % Radius Kreis
X = [-1,1,1,-1,-1]; Y = [-1,-1,0,0,-1];
fill(X,Y,'y','edgecolor','y'), hold on
X = [-1,2.5]; Y = [0, 0];
plot(X,Y,'k','linewidth',2), hold on
X = [1,2.5]; Y = [6,6];
plot(X,Y,'k','linewidth',2), hold on
X = [2,2]; Y = [3.6,6];
arrow(X,Y,c,d,'k',1)
X = [2,2]; Y = [2.4,0];
arrow(X,Y,c,d,'k',1)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
X = [0, 0]; Y = [-0.5,7];
plot(X,Y,'k-.','linewidth',2),hold on
X = [1,1]; Y = [7.5,6];
arrow(X,Y,c,d,'k',2)
% -- Kurve -------------------5
ll = 6; kappa = 1; sqlamda = pi/(2*ll);
Y = linspace(0,6,20);
X = kappa*(1 - cos(sqlamda*Y));
plot(X,Y,'k','linewidth',3), hold on
X = [0,0]; Y = [0,-0.5];
plot(X,Y,'k','linewidth',3), hold on
%-- Rahmen ---------------
X = [-2,4,4,-2,-2]; Y = [-2,-2,8,8,-2];
plot(X,Y,'k','linewidth',2), hold on
%get(h)
axis equal tight, grid on
text(1.5,7,'F','fontsize',25)
text(2,3,'l','fontsize',25)

axis off, grid off
