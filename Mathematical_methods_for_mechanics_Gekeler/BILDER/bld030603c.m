function bld030603c
clf
% -- B ----------------------
RBX = [-1.5,    0,  0, -1.5, - 1.5];
RBY = [-0.5, -0.5, 1,   1, -0.5];
fill(RBX,RBY,'y'), hold on
% -- Randkurve ------------------------------
MX = 6; MY = 9; R = sqrt(MX*MX + (MY - 1)^2);
A = pi + 0.926; B = pi + 1.25;
TT = linspace(A,B,40);
X3 = MX + R*cos(TT);
Y3 = MY + R*sin(TT);
% -- A -------------------------
RAX = [X3, 3,3,0,0];
RAY = [Y3,-0.5,2.5, 2.5,2.5];
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
X2 = [-0.7,0.7];
Y2 = 1 - 2*X2;
plot(X2,Y2,'k:','linewidth',2), hold on
% -- Randkurve ------------------------
plot(X3,Y3,'k','linewidth',2), hold on

% -- Schnittpunkt ---------------
circle(0,1,0.05,'w')
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