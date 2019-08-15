function bild24
% Long wave on a beach
% cf. J.Petera/V.Nassehi: Int. J. Numer. Meth. Eng. 39, 4159-4182
% Geometrie
clc
% -- Eckpunkte
clf, hold on
plot(0,-3,'w.'), hold on
plot(40,3,'w.'), hold on
axis tight, axis manual, grid on
c = 1; d = 0.1;
X = [17,0]; Y = [-2.5,-2.5];
arrow(X,Y,c,d,'k',2), hold on
X = [23,40]; Y = [-2.5,-2.5];
arrow(X,Y,c,d,'k',2), hold on

c = 0.18; d = 0.5;
XA= [15,15]; YA = [1.3,2];
arrow(XA,YA,c,d,'k',2), hold on
XA= [15,15]; YA = [0.7,0];
arrow(XA,YA,c,d,'k',2), hold on

c = 0.18; d = 0.5;
XA= [30,30]; YA = [-0.3,0];
arrow(XA,YA,c,d,'k',2), hold on
XA= [30,30]; YA = [-0.7,-1];
arrow(XA,YA,c,d,'k',2), hold on

X = [0,40]; Y = [0,0];
plot(X,Y,'k--','linewidth',2), hold on
X = [13,40]; Y = [2,2];
plot(X,Y,'k--','linewidth',2), hold on
NN = 100;
A0 = 0.1; alfa = 1/30; XX = linspace(0,40,NN+1);
AUX = 0.5*sqrt(3*A0)*(XX - 1/alfa);
Z0 = A0./(cosh(AUX).*cosh(AUX)) ;
U0 = -(1 + 0.5*A0)*Z0./(alfa*XX + Z0); U0 = [0,U0(2:NN+1)];
H0 = linspace(0,-4/3,NN+1);
plot(XX,20*Z0,'r','linewidth',2), hold on
plot(XX,20*U0,'g','linewidth',2), hold on
plot(XX,H0,'b','linewidth',2), hold on
text(12,1,'a_0 = 0.1','fontsize',20)
text(17,-2.5,'40 m','fontsize',20)
text(28,-0.5,'1 m','fontsize',15)
text(10,-1,'sea bed','fontsize',20)
text(2,-0.5,'1:30','fontsize',15)
text(33,1.5,'\zeta_0','fontsize',20)
text(33,-1.5,'u_0','fontsize',20)
axis off
clear
