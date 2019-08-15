function ellipse
% Figure of an ellipse

clc, clf
a  = 2; b = 1.2;     % Hauptachses
dd = 0.5;           % Randstreifen
c  = 0.15; d = 0.05; % Form der Pfeilspitze
rr = 0.04;          % Radius der Markierungskreise
e = sqrt(a*a - b*b);
% -- Achsen ------------
X1 = [-a-dd;0]; Y1 = [0;0];
plot(X1,Y1,'k','linewidth',2), hold on
X1 = [e;a+dd]; Y1 = [0;0];
arrow(X1,Y1,c,d,'k',2);
X1 = [0;0]; Y1 = [-b-dd;b+dd];
arrow(X1,Y1,c,d,'k',2);
X1 = [0.3;0]; Y1 = [0;0];
arrow(X1,Y1,c,d,'k',2)
X1 = [1.35;e]; Y1 = [0;0];
arrow(X1,Y1,c,d,'k',2)
X1 = [e;e]; Y1 = [0;b*b/a];
plot(X1,Y1,'k:', 'linewidth',2), hold on
XX = linspace(-a,a,200);
YY = b*sqrt(1 - XX.*XX/(a*a));
plot(XX,YY,'k','linewidth',2), hold on
plot(XX,-YY,'k','linewidth',2), hold on
%
X3 = [e;-1]; Y3 = [0; b*sqrt(1 - 1/(a*a))];
plot(X3,Y3,'k'), hold on
X4 = [-1;-e]; Y4 = [b*sqrt(1-1/(a*a));0];
plot(X4,Y4,'k'), hold on
XS = [0,e]; YS = [b,0];
plot(XS,YS,'k--'), hold on


% -- Tangente und Normale ---------
X = - 1; AD = 0.8;
A = [X;b*sqrt(1 - X*X/(a*a))];
DF = -b*X/(a*sqrt(a*a - X*X));
X5 = [X-AD;X+AD];
Y5 = [A(2)-AD*DF;A(2)+AD*DF];
plot(X5,Y5,'k','linewidth',2),hold on
AD = 0.2;
X5 = [X-AD;X+AD];
Y5 = [A(2)+AD/DF;A(2)-AD/DF];
plot(X5,Y5,'k','linewidth',2),hold on

circle(e,0,rr,'w')
circle(-e,0,rr,'w')
circle(X3(2),Y3(2),rr,'w')
circle(e,b*b/a,rr,'w')
circle(a,0,rr,'w')
circle(-a,0,rr,'w')
circle(0,b,rr,'w')
circle(0,-b,rr,'w')

text(0.95,0.65,'a','fontsize',22)
text(-0.25,0.5,'b','fontsize',22)
text(-1,-0.2,'a','fontsize',22)
text(0.35,0.05,'e = \epsilon a','fontsize',22)
text(2,1,'p','fontsize',22)
text(1.5,-0.3,'F_2','fontsize',22)
text(-1.7,-0.3,'F_1','fontsize',22)

X1 = [1.95;1.65]; Y1 = [0.9;0.4];
arrow(X1,Y1,c,d,'k',1)
% -- Bildecken -----------
XR = [-a-dd,a+dd,a+dd,-a-dd,-a-dd];
YR = [-b-dd,-b-dd,b+dd,b+dd,-b-dd];
plot(XR,YR,'k','linewidth',2), hold on
axis equal tight
grid on
axis off
