% BILD039
clf
% -- Spline-Daten -----------------------
f = [1, 3.2/4, 1.8/4, 2.5/4, 3.25/4,  2.5/4, 1.2];
LF = length(f);
% Achtung: Spline wird fuer Abszissen 0:LF-1 berechnet!!!
x = linspace(0,LF-1,40);
s = spline_n(f,x);
x = x/3;              % Umskalieren!!
plot(x,s,'b','linewidth',2), hold on
% -- X-Achse --------------------------
X = [0,2]; Y = [0,0];
c = 0.07; d = 0.03;
arrow(X,Y,c,d,'k',2)
% ----------------------------------
X = [0,2]; Y = [1,1];
plot(X,Y,'k:','linewidth',2), hold on
% -------------------------------
%X = [0,2]; Y = [1,0.2];
%plot(X,Y,'c:','linewidth',2), hold on
% ----------------------------------
X1 = [0,2]; Y1 = [1,0.65];
plot(X1,Y1,'k','linewidth',2), hold on
X3 = [1.75,1.75]; Y3 = [-0.1,0.9];
plot(X3,Y3,'k--','linewidth',2), hold on
X4 = [1.43,1.43]; Y4 = [-0.1,1.1];
plot(X4,Y4,'k--','linewidth',2), hold on
X5 = [1.2,1.2]; Y5 = [-0.1,1.1];
plot(X5,Y5,'k--','linewidth',2), hold on
X6 = [0.6,0.6]; Y6 = [-0.1,1.1];
plot(X6,Y6,'k--','linewidth',2), hold on
X6 = [0.3,0.3]; Y6 = [-0.1,1.1];
plot(X6,Y6,'k--','linewidth',2), hold on
X7 = linspace(0,0.7,20);
Y7 = 1 - 0.4*X7 + (2/3)*X7.*X7;
plot(X7,Y7,'k','linewidth',2), hold on
% -- Rahmen --------------------------
RX = [-0.05,2.05,2.05,-0.05,-0.05];
RY = [-0.2,-0.2,1.25,1.25,-0.2];
plot(RX,RY,'k','linewidth',2)
text(1.7,1.1,'\phi(\sigma)','fontsize',22)
text(0.7,1.1,'\psi(\sigma)','fontsize',22)
text(1.8,0.58,'g(\sigma)','fontsize',22)
text(0.65,-0.08,'\rho','fontsize',22)
text(0.32,-0.08,'\rho/2','fontsize',22)
text(1.85,-0.08,'\sigma','fontsize',22)
axis equal
axis off
