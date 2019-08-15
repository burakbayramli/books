function bld040409a
% Beispiel 9, Brachistochrone
%load daten09a X Parmeter
clf
% -- X-Achse ----------------
c = 0.05; d = 0.015;
X1 = [0,1.2]; Y1 = [0, 0];
arrow(X1,Y1,c,d,'k',2)
% -- Y-Achse ---------------
c = 0.04; d = 0.015;
X2 = [0, 0]; Y2 = [0, -0.7];
arrow(X2,Y2,c,d,'k',2)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Loesung I-------------
load daten09a X Parmeter
n  = Parmeter(1); g = Parmeter(2); B1  = Parmeter(3); C = Parmeter(4);
X1  = X(1:n+1); X2  = -X(n+2:2*(n+1)); U = X(2*n+3:3*(n+1));
T1  =  X(3*n+4)
plot(X1,X2,'k','linewidth',2), hold on
circle(X1(n+1),X2(n+1),0.015,'w')
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Loesung II-------------
load daten09b X Parmeter
n  = Parmeter(1); g = Parmeter(2); B1  = Parmeter(3); C = Parmeter(5);
X1  = X(1:n+1); X2  = -X(n+2:2*(n+1)); U = X(2*n+3:3*(n+1));
T2  =  X(3*n+4)
plot(X1,X2,'k','linewidth',2), hold on
circle(X1(n+1),X2(n+1),0.015,'w')
% -- Restriktion -----------
X3 = linspace(0,1,20); Y3 = C + X3/2;
plot(X3,-Y3,'g','linewidth',2), hold on;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Loesung III-------------
load daten09c X Parmeter
n  = Parmeter(1); g = Parmeter(2); B1  = Parmeter(3); C = Parmeter(5);
X1  = X(1:n+1); X2  = -X(n+2:2*(n+1)); U = X(2*n+3:3*(n+1));
T3  =  X(3*n+4)
plot(X1,X2,'k','linewidth',2), hold on
circle(X1(n+1),X2(n+1),0.015,'w')
% -- Restriktion -----------
X3 = linspace(0,1,20); Y3 = C + X3/2;
plot(X3,-Y3,'b','linewidth',2), hold on;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Loesung IV -------------
load daten09d X Parmeter
n  = Parmeter(1); g = Parmeter(2); B1  = Parmeter(3);
C = Parmeter(5);
X1  = X(1:n+1); X2  = -X(n+2:2*(n+1)); U = X(2*n+3:3*(n+1));
T4  =  X(3*n+4)
plot(X1,X2,'k','linewidth',2), hold on
circle(X1(n+1),X2(n+1),0.015,'w')
% -- Restriktion -----------
X3 = linspace(0,1,20); Y3 = 0.1 + X3/2;
plot(X3,-Y3,'r','linewidth',2), hold on;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% -- Exakte Loesung ohne Restriktionen ------
omga = sqrt(pi*g/(4*B1));
T = sqrt(pi*B1/g);
TT = linspace(0,T,30);
X5 = B1*2*(omga*TT - sin(2*omga*TT)/2)/pi;
Y5 = B1*2*(sin(omga*TT)).^2/pi;
plot(X5,-Y5,'b','linewidth',2), hold on
circle(X5(30),-Y5(30),0.015,'w')
circle(0,0,0.015,'w')

plot(-0.05,-0.8,'w.'), hold on
plot(1.25,0.05,'w.'), hold on
axis equal tight, grid on
%ZEIT = T
text(0.04,-0.65,'x_2','fontsize',22)
text(1.13,-0.06,'x_1','fontsize',22)
%ylabel('- x_2','fontsize',18)
grid off
