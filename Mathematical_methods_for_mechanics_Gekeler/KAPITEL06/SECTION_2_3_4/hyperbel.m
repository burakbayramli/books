function hyperbel
% Figure of an hyperbola

clc, clf
a  = 1; b = 1.3;    % Hauptachsen
dd = 0.5;           % Randstreifen
c  = 0.24; d = 0.08; % Form der Pfeilspitze
rr = 0.06;          % Radius der Markierungskreise
p = b*b/a; e = sqrt(a*a + b*b);
p = 1;
% -- X-Achse ------------
X1 = [-2.8;0]; Y1 = [0;0];
plot(X1,Y1,'k','linewidth',2), hold on
X1 = [0;a]; Y1 = [0;0];
plot(X1,Y1,'k--','linewidth',2), hold on
X1 = [a;2.8]; Y1 = [0;0];
arrow(X1,Y1,c,d,'k',2)
% -- Y-Achse --------------------
X1 = [0;0]; Y1 = [-2.8;2.8];
arrow(X1,Y1,c,d,'k',2);
% -- Asymptoten ------------
DA = 2;
X1 = [-DA;DA]; Y1 = [-DA*b/a;DA*b/a];
plot(X1,Y1,'k--','linewidth',2), hold on
X1 = [-DA;DA]; Y1 = [DA*b/a;-DA*b/a];
plot(X1,Y1,'k--','linewidth',2), hold on
% -- Hyperbel -------------------
XX = linspace(a,2.4,100);
YY = b*sqrt(XX.*XX/(a*a)- 1);
plot(XX,YY,'k','linewidth',2),hold on
plot(XX,-YY,'k','linewidth',2),hold on
plot(-XX,YY,'k','linewidth',2),hold on
plot(-XX,-YY,'k','linewidth',2),hold on
XP = e;
YP = b*sqrt(XP.*XP/(a*a)- 1);
X2 = [e;e]; Y2 = [0;YP];
plot(X2,Y2,'k:','linewidth',2), hold on
XC = [a;a]; YC = [0;b];
plot(XC,YC,'k--','linewidth',2), hold on
% -- Tangente und Normale ---------
XX = -1.4;
YY = b*sqrt(XX.*XX/(a*a)- 1);
X1 = [-e;XX]; Y1 = [0;YY];
plot(X1,Y1,'k'), hold on
LL = sqrt(YY^2 + (XX+e)^2);
X1 = [e;XX]; Y1 = [0;YY];
plot(X1,Y1,'k'), hold on
LAENGE = sqrt((X1(2)-X1(1))^2 + (Y1(2)-Y1(1))^2);
X3 = XX - LL*(X1(2) - X1(1))/LAENGE;
Y3 = YY - LL*(Y1(2) - Y1(1))/LAENGE;
plot([-e,X3],[0,Y3],'k'),hold on
X4 = (X3-e)/2; Y4 = Y3/2;
TN = [X4-XX;Y4-YY];
DA = 0.5;
X5 = [XX-  DA*TN(1); XX + 2*TN(1)];
Y5 = [YY - DA*TN(2); YY + 2*TN(2)];
plot(X5,Y5,'k'), hold on
circle(0,0,rr,'w')
circle(-e,0,rr,'w')
circle(e,0,rr,'w')
circle(XX,YY,rr,'w')
circle(X3,Y3,rr,'w')
circle(XP,YP,rr,'w')
circle(a,b,rr,'w')
circle(a,0,rr,'w')
circle(X4,Y4,rr,'w')
text(0.36,-1.5,'a','fontsize',18)
X1 = [0.5,0.5]; Y1 = [-1.3,-0.1];
arrow(X1,Y1,c,d,'k',1)
text(1.7,1,'p','fontsize',18)
text(0.36,1.5,'b','fontsize',18)
X1 = [0.5,0.9]; Y1 = [1.3,0.6];
arrow(X1,Y1,c,d,'k',1)
text(1.5,-0.5,'F_1','fontsize',18)
text(-1.8,-0.5,'F_2','fontsize',18)
% -- Bildecken -----------

plot(-3,-3,'w.','markersize',3), hold on
plot(3,3,'w.','markersize',3), hold on
axis equal tight
grid on
axis off
