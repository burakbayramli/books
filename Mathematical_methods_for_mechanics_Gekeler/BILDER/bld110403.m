function bld110403
clc, clear
clf, hold on
% -- Daten ---------------------------
rw = 1; RW = 2; alfa = pi/4;
RadAG = RW*cos(alfa); RadBG = rw*cos(alfa);
RadAK = sqrt((RW+rw)^2 + RadBG^2 - 2*(RW + rw)*RadBG*cos(alfa));
RadBK = sqrt((RW+rw)^2 + RadAG^2 - 2*(RW + rw)*RadAG*cos(alfa));
% -- Rahmen -----------------------
LU = [-2.5,-1.5]; RO = [1.5,1.5];
XR = [LU(1),RO(1),RO(1),LU(1),LU(1)];
YR = [LU(2),LU(2),RO(2),RO(2),LU(2)];
plot(XR,YR,'k','linewidth',2), hold on
delta = 0.01;
plot(LU(1)-delta,LU(2)-delta,'w.','markersize',3), hold on
plot(RO(1)+delta,RO(2)+delta,'w.','markersize',3), hold on
axis equal tight, axis manual
grid on
% Strecken ------------------------
beta =  - alfa;
X1 = [-RW, -RW+RadAG*cos(beta)]; Y1 = [0,RadAG*sin(beta)];
plot(X1,Y1,'r','linewidth',2), hold on
beta = pi - alfa;
X2 = [rw, rw+RadBG*cos(beta)]; Y2 = [0,RadBG*sin(beta)];
plot(X2,Y2,'r','linewidth',2), hold on
X3 = [X1(2),X2(2)]; Y3 = [Y1(2),Y2(2)];
plot(X3,Y3,'b','linewidth',2), hold on
aa = 0.2;
% -- Grosses Zahnrad ---------------
TT = linspace(-pi/2-aa,pi/2+aa,40);
X = -RW + RW*cos(TT); Y = RW*sin(TT);
plot(X,Y,'k--','linewidth',2), hold on
X = -RW + RadAG*cos(TT); Y = RadAG*sin(TT);
plot(X,Y,'k','linewidth',2), hold on
X = -RW + RadAK*cos(TT); Y = RadAK*sin(TT);
plot(X,Y,'g','linewidth',2), hold on
% -- Kleines Zahnrad ----------------
aa = 0.4;
TT = linspace(pi/2-aa,3*pi/2+aa,80);
X = rw + rw*cos(TT); Y = rw*sin(TT);
plot(X,Y,'k--','linewidth',2), hold on
X = rw + RadBG*cos(TT); Y = RadBG*sin(TT);
plot(X,Y,'k','linewidth',2), hold on
X = rw + RadBK*cos(TT); Y = RadBK*sin(TT);
plot(X,Y,'g','linewidth',2), hold on

% -- Evolvente Zahnrad A -------------
m = 1; RadAK = RW + m;
alfa1 = acos(RW*cos(alfa)/(RW + m));
alfa0 = tan(alfa) - alfa;
RG = RW*cos(alfa);
TT = linspace(alfa0,-alfa1,20);
X4 = RW + RG*(cos(TT) - abs(TT-alfa0).*sin(TT));
Y4 = RG*(sin(TT) + abs(TT- alfa0).*cos(TT));
plot(X4,Y4,'r','linewidth',2), hold on

% -- Evolvente Zahnrad B -------------
m = 1; RadBK = RW + m;
alfa1 = pi + acos(RW*cos(alfa)/(RW + m));
alfa0 = pi - (tan(alfa) - alfa);
RG = RW*cos(alfa);
TT = linspace(alfa0,alfa1,20);
X4 = RW + RG*(cos(TT) - abs(TT-alfa0).*sin(TT));
Y4 = RG*(sin(TT) + abs(TT- alfa0).*cos(TT));
%plot(X4,Y4,'r','linewidth',2), hold on
% -- Weisse Markierungen ---------------
RR = 0.04;
circle(0,0,RR,'w')
circle(-RW,0,RR,'w')
circle(rw,0,RR,'w')
circle(X3(1),Y3(1),RR,'w');
circle(X3(2),Y3(2),RR,'w');
text(-0.5,1,'Waelzkreise','fontsize',22)
text(-2.3,1.2,'Grundkreis A','fontsize',22)
axis off
