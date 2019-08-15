% BILD052, Slater-Bedingung
clf
% -- Kegel ----
X3 = [0,2,2,0,0];
Y3 = [0,0,2,2,0];
fill(X3,Y3,'y'), hold on

PHI = - pi/4; %Drehwinkel fuer Ellipse
CC = [cos(PHI), - sin(PHI); sin(PHI), cos(PHI)];
T = linspace(0,2*pi,40);
A = 1.7;
X = A*cos(T);
Y = A*sin(T)/2;
X1 = CC*[X;Y];
C = 4.25; D = 5.18;
TT = linspace(C,D,40);
X4 = A*cos(TT);
Y4 = A*sin(TT)/2;
X5 = CC*[X4;Y4];
X6 = [X5(1,:),0,X5(1,1)];
Y6 = [X5(2,:),0,X5(2,1)];
fill(X6,Y6,'y'), hold on
c = 0.2;d = 0.07;

plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on

% -- X-Achse ------------
X = [-2,2];
Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
% -- Y- Achse ---------
X = [0,0];
Y = [-2,2];
arrow(X,Y,c,d,'k',2), hold on
% -- Rahmen ---------------
RX = [-2,2,2,-2,-2];
RY = [-2,-2,2,2,-2];
plot(RX,RY,'k','linewidth',2)

axis equal tight
%grid on
text(0.9,1,'K','FontSize',36)
text(-0.6,-0.3,'A','FontSize',30)
text(-0.15,-0.5,'g(C)','FontSize',30)
text(-0.5,0.5,'g(C)','Fontsize',30)
axis off
