% BILD008
clf
% -- Ellipse ----------------------------------
PHI = - pi/4; %Drehwinkel fuer Ellipse
CC = [cos(PHI), - sin(PHI); sin(PHI), cos(PHI)];
T = linspace(0,2*pi,40);
A = 2.5;
MITTE = [1;1];
X = A*cos(T);
Y = A*sin(T)/3;
X1 = MITTE*ones(1,length(X)) + CC*[X;Y];
fill(X1(1,:),X1(2,:),'y'), hold on
% -- Dualer Kegel -----------------------
PHI1 = -0.36;
PHI2 = 1.93;
R1 = 3.75; R2 = 3.75;
X3 = [0,-R1*sin(PHI1)];
Y3 = [0,R1*cos(PHI1)];
X4 = [0,R2*sin(PHI2)];
Y4 = [0,-R2*cos(PHI2)];
plot(X4,Y4,'r'), hold on
plot(X3,Y3,'r'), hold on
X5 = [0,X4(2),3.5,X3(2),0];
Y5 = [0,Y4(2),3.5,Y3(2),0];
fill(X5,Y5,'y'), hold on



plot(X1(1,:),X1(2,:),'k','linewidth',2), hold on
c = 0.15; d = 0.07;

% -- Kegel ----
PHI1 = -0.36; R = 3;
X3 = [0,R*cos(PHI1)];
Y3 = [0,R*sin(PHI1)];
arrow(X3,Y3,c,d,'k',2)
PHI2 = 1.93;
X4 = [0,R*cos(PHI2)];
Y4 = [0,R*sin(PHI2)];
arrow(X4,Y4,c,d,'k',2)
% -- Dualer Kegel ---------------------
X3 = [0,-R*sin(PHI1)];
Y3 = [0,R*cos(PHI1)];
arrow(X3,Y3,c,d,'k',2)
X3 = [0,R*sin(PHI2)];
Y3 = [0,-R*cos(PHI2)];
arrow(X3,Y3,c,d,'k',2)

% -- X-Achse ------------
X = [-0.5,3.2];
Y = [0,0];
arrow(X,Y,c,d,'k',2), hold on
% -- Y- Achse ---------
X = [0,0];
Y = [-0.5,3.2];
arrow(X,Y,c,d,'k',2), hold on
% -- Rahmen ---------------
RX = [-1.5,3.5,3.5,-1.5,-1.5];
RY = [-1.5,-1.5,3.5,3.5,-1.5];
plot(RX,RY,'k','linewidth',2)

axis equal
grid on
text(1.6,0,'M','FontSize',24)
text(-0.2,1.9,'M','FontSize',24)
text(0.9,1,'M','FontSize',24)
text(2,2,'M_d','FontSize',30)
text(3,-0.3,'x_1','FontSize',24)
text(0.1,3,'x_2','FontSize',24)
axis off
