function fig0408
% Figure 4.8

disp(' Call first DEMO1-4 with all options ! ')

clf
% -- X-Achse --------------
c = 0.5; d = 0.2;
X1 = [-7,7]; Y1 = [0, 0];
arrow(X1,Y1,c,d,'k',2)
% -- Y-Achse -------------
X1 = [0, 0]; Y1 = [-7, 7];
arrow(X1,Y1,c,d,'k',2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TT = linspace(0,pi,40);
X2 = cos(TT);
Y2 = - sin(TT);
plot(1+X2,Y2,'k'), hold on
plot(3+X2,Y2,'k'), hold on
plot(5+ X2,Y2,'k'), hold on
plot(-1 + X2,-Y2,'k'), hold on
plot(-3 + X2,-Y2,'k'), hold on
plot(-5 + X2,-Y2,'k'), hold on

load daten04a X Parmeter
n  = Parmeter(1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4);
plot(X1,X2,'k'), hold on
plot(X1,X2,'.','Markersize',6), hold on
TIME1 = X4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten04b X Parmeter
n  = Parmeter(1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4);
plot(X1,X2,'k'), hold on
plot(X1,X2,'.','Markersize',6), hold on
TIME2 = X4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten04c X Parmeter
n  = Parmeter(1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4);
plot(X1,X2,'k'), hold on
plot(X1,X2,'.','Markersize',6), hold on
TIME3 = X4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load daten04d X Parmeter
n  = Parmeter(1);
X1 = X(1:n+1); X2 = X(n+2:2*(n+1)); X3 = X(2*n+3:3*(n+1));
X4 = X(3*n+4);
plot(X1,X2,'k'), hold on
plot(X1,X2,'.','Markersize',6);
TIME4  = X4
text(3,5.5,'u = - 1','fontsize',20)
text(3,-5.5,'u = + 1','fontsize',20)
axis equal tight
grid off
