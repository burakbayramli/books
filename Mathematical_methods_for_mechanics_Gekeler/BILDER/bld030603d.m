% BILD057
clf
% -- Randkurve I------------------------------
MX = 5; MY = 5; R = sqrt((MX - 0.5)^2 + (MY - 0.5)^2);
A = pi + 0.79; B = pi + 1.045;
TT = linspace(A,B,40);
X3 = MX + R*cos(TT);
Y3 = MY + R*sin(TT);
% -- Randkurve II------------------------------
MX = 4.5; MY = 6.5; R = sqrt(50);
A = pi + 0.6; B = pi + 0.79;
TT = linspace(A,B,40);
X4 = MX + R*cos(TT);
Y4 = MY + R*sin(TT);
% -- Randkurve III------------------------------
X5 = linspace(-0.5,0.5,11);
Y5 = [1.5,1.43,1.37,1.33,1.31,1.28,1.25,1.175,1,0.65,0.5];
plot(X5,Y5,'b','linewidth',1), hold on
plot(X5,Y5,'.','markersize',6), hold on
% -- B ----------------------
RBX = [-1.5,    0,  0, -1.5, - 1.5];
RBY = [-0.5, -0.5, 1,   1, -0.5];
fill(RBX,RBY,'y'), hold on

% -- A -------------------------
RAX = [X4,X5,X3, 3,3,0,0];
RAY = [Y4,Y5,Y3,-0.5,2.5, 2.5,2.5];
fill(RAX,RAY,'y'), hold on

% -- X-Achse ----------------------
X = [-1.4,2.9]; Y = [0,0];
c = 0.15; d = 0.07;
arrow(X,Y,c,d,'k',2)
% -- Y-Achse --------------------------
X = [0,0]; Y = [-0.4,2.4];
arrow(X,Y,c,d,'k',2)
% -- Schnittpunkt + Tangente ---------------
X1 = [-1.5, 1.5];
Y1 = 1 - X1;
plot(X1,Y1,'k','linewidth',2), hold on
% -- Randkurve ------------------------
X6 = [X4,X5,X3];
Y6 = [Y4,Y5,Y3];
plot(X6,Y6,'k','linewidth',2), hold on

% -- Schnittpunkt ---------------
circle(0.5,0.5,0.05,'w')
circle(-0.5,1.5,0.05,'w')
circle(0,1,0.05,'w')
circle(0,1.28,0.05,'w')

% --Rahmen --------------------------
X5 = [-1.5,3,3,-1.5,-1.5];
Y5 = [-0.5,-0.5,2.5,2.5,-0.5];
plot(X5,Y5,'k','linewidth',2)


text(1.5,1.5,'A','fontsize',48)
text(-1,0.4,'B','fontsize',32)

text(0.1,2.3,'\rho','fontsize',24)
text(2.6,0.2,'\Gamma','fontsize',24)
grid on
axis equal
axis off
