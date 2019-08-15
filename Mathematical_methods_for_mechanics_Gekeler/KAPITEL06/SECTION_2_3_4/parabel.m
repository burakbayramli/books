function parabel
% Figure of a parabola

clc, clf
a  = 2; b = 1.2;     % Hauptachsen
dd = 0.5;           % Randstreifen
c  = 0.15; d = 0.07; % Form der Pfeilspitze
rr = 0.04;          % Radius der Markierungskreise
p = b*b/a; e = sqrt(a*a - b*b);
p = 1;
% -- X-Achse ------------
X1 = [-1.8;1.8]; Y1 = [0;0];
arrow(X1,Y1,c,d,'k',2)
% -- Y-Achse --------------------
X1 = [0;0]; Y1 = [p;2.2];
arrow(X1,Y1,c,d,'k',2);
X1 = [0;0]; Y1 = [-2.2;0];
plot(X1,Y1,'k','linewidth',2), hold on
X1 = [0;0]; Y1 = [0;p];
plot(X1,Y1,'k:','linewidth',2), hold on
% -- Parabel -------------------
YY = linspace(-2.2,2.2,100);
XX = 0.5*p  - YY.^2/(2*p);
plot(XX,YY,'k','linewidth',2),hold on
X6 = [p;p];
Y6 = [-2.2;2.2];
plot(X6,Y6,'k--','linewidth',2), hold on
Y8 = [0;1.9];
X8 = [0; 0.5*p-Y8(2)^2/(2*p)];
plot(X8,Y8,'k'), hold on
X9 = [X8(2),p];
Y9 = [Y8(2);Y8(2)];
plot(X9,Y9,'k'), hold on
% -- Tangente und Normale ---------
A = X8(2); B = Y8(2);
C = 0.5*p; D = B/2;
DF = (D-B)/(C-A);
DA = 0.7;
X4 = [A-DA;C+DA];
Y4 = [B-DF*DA;D+DF*DA];
plot(X4,Y4,'k'), hold on
X5 =[0;p]; Y5 = [0;B];
plot(X5,Y5,'g'), hold on

circle(0,0,rr,'w')
circle(p/2,0,rr,'w')
circle(0,p,rr,'w')
circle(X8(2),Y8(2),rr,'w')
circle(p,Y8(2),rr,'w')
circle(p/2,Y8(2)/2,rr,'w')

text(-1,-0.5,'p','fontsize',18)
X1 = [-0.7,-0.1]; Y1 = [-0.5,0.5];
arrow(X1,Y1,c,d,'k',1)
text(1.2,-1.2,'p/2','fontsize',18)
X1 = [1.4,0.25]; Y1 = [-1,-0.1];
arrow(X1,Y1,c,d,'k',1)
% -- Bildecken -----------
plot(-2,-2.5,'w.','markersize',3), hold on
plot(2,2.5,'w.','markersize',3), hold on
axis equal tight
grid on
axis off
