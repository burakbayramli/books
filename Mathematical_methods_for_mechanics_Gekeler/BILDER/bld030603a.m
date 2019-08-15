function bld030603a
clf
% -- Spline-Daten -----------------------
%f = [1, 3.2/4, 1.8/4, 2.5/4, 3.25/4,  2.5/4, 1.2];
%LF = length(f);
% Achtung: Spline wird fuer Abszissen 0:LF-1 berechnet!!!
%x = linspace(0,LF-1,40);
%s = spline_n(f,x);
%x = x/3;              % Umskalieren!!
%plot(x,s,'b','linewidth',2), hold on
% -- B ----------------------
RBX = [-1.5,    0,  0, -1.5, - 1.5];
RBY = [-0.5, -0.5, 1,   1, -0.5];
fill(RBX,RBY,'y'), hold on
% -- A -------------------------
M1 = 4;
M2 = 5.5;
R = 6;
A = pi + 0.522;
B = pi + 1.4;
TT = linspace(A,B,40);
X = M1 + R*cos(TT);
Y = M2 + R*sin(TT);
ECKE = [3,2.5];
RAND1 = [X,ECKE(1),X(1)];
RAND2 = [Y,ECKE(2),Y(1)];
fill(RAND1,RAND2,'y'), hold on
% -- omega ---------
M1 = 4;
M2 = 5.5;
R = 6;
A = pi + 0.522;
B = pi + 1.4;
TT = linspace(A,B,40);
X = M1 + R*cos(TT);
Y = M2 + R*sin(TT);
plot(X,Y,'k','linewidth',2), hold on
% --------------------------------------

% -- Schnittpunkt + Tangente ---------------
T1 = pi+0.838;
X1 = M1 + R*cos(T1);
Y1 = M2 + R*sin(T1);
X2 = X1 - R*sin(T1)*[-0.3,0.4];
Y2 = Y1 + R*cos(T1)*[-0.3,0.4];
plot(X2,Y2,'k','linewidth',3),hold on
% -- X-Achse ----------------------
X = [-1.4,2.9]; Y = [0,0];
c = 0.15; d = 0.07;
arrow(X,Y,c,d,'k',2)
% -- Y-Achse --------------------------
X = [0,0]; Y = [-0.4,2.4];
arrow(X,Y,c,d,'k',2)
RR = 0.05;
circle(X1,Y1,RR,'w')

% -- weiterer Punkt + Tangente -------------
T1 = pi+1.3;
X3= M1 + R*cos(T1);
Y3 = M2 + R*sin(T1);
X4 = X3 - R*sin(T1)*[-0.63,0.1];
Y4 = Y3 + R*cos(T1)*[-0.63,0.1];
plot(X4,Y4,'k:','linewidth',2),hold on
circle(X3,Y3,RR,'w')

% -- weiterer Punkt + Tangente -------------
T1 = pi+0.65;
X = M1 + R*cos(T1);
Y = M2 + R*sin(T1);
X1 = X - R*sin(T1)*[-0.12,0.45];
Y1 = Y + R*cos(T1)*[-0.12,0.45];
plot(X1,Y1,'k:','linewidth',2),hold on
circle(X,Y,RR,'w')

circle(0,0.838,RR,'w')
circle(0,0.39,RR,'w')

% --Rahmen --------------------------
X5 = [-1.5,3,3,-1.5,-1.5];
Y5 = [-0.5,-0.5,2.5,2.5,-0.5];
plot(X5,Y5,'k','linewidth',2)
text(1.5,1.5,'A','fontsize',48)
text(-1,0.4,'B','fontsize',32)
text(0.1,2.3,'\rho','fontsize',24)
text(2.6,0.2,'\Gamma','fontsize',24)
text(1,0.5,'\omega(u)','fontsize',24)
text(0.1,1.1,'\omega(0)','fontsize',24)

axis equal tight
axis off
%grid on
